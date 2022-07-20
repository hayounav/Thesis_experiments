package ox.cads.collection
import java.util.concurrent.atomic._
import scala.reflect.ClassTag
// import ox.cads.util.ThreadID
import ox.cads.util.{Profiler,Counter} 

/** An implementation of a HashMap based upon "Boosting Multi-Core
  * Reachability Performance with Shared Hash Tables", by Alfons Laarman, 
  * Jaco van de Pol and Michael Weber, extended to allow deletion and 
  * resizing. 
  * @param size the initial size of each shard; this must be a power of 2.
  * @param p the number of workers.
  */
class LvdPWResizeMap[A: ClassTag, B: ClassTag](size: Int, p: Int) 
extends HashMap[A, B]{
  def this(size: Int) = this(size, 8)
  def this() = this(256)

  // TODO: experiment with different resizing tactics.  Maybe only help with
  // resizing on a write operation.

  // Check size is a power of 2
  private var i = size
  while(i > 2){ assert(i%2 == 0); i = i/2 }

  // Maximum load factor
  private val MaxLoadFactor = 600 // represents 0.6

  /** Each object of the State class represents a coherent state */
  private class State(val size: Int){
    // println(size)
    val hashes = new AtomicIntegerArray(size)
    val keys = new Array[A](size)
    val data = new Array[B](size)

    // Number of non-null keys
    val count = new Counter(p)
    // Threashold for resizing (need to use Long for calculating this!)
    val threshold = (size.toLong * MaxLoadFactor / 1000).toInt

    // Counter indicating next block for copying of this into a new hash table
    val nextCopy = new AtomicInteger(0)
    // Counter indicating how many entries have been copied
    val copied = new AtomicInteger(0)
    // Is this currently being resized?  The worker that sets this creates the
    // next state.
    val resizing = new AtomicBoolean(false)
  }

  private var state = new AtomicReference(new State(size))
  private var newState = new AtomicReference(state.get)
  // state is the current State object.  newState normally equals state,
  // except during resizing, when it is the new state!

  // Abstractly, each hashes(i) contains one of the following values: Empty
  // (indicating an empty slot), write(h) indicating that a value with hash h
  // is currently being written, done(h) indicating that a value with hash h
  // has been inserted, or deleted(h) indicating that a value with hash h has
  // been deleted. The state transitions in the sequence Empty -> write(h) ->
  // done(h); and subsequently transitions done(h) -> write(h) -> done(h), or
  // done(h) -> deleted(h) -> write(h) -> done(h).  The corresponding key
  // transitions from its initial value when the hash is first write(h), and
  // subsequently doesn't change.  The data value is changed to a new value
  // only when the hash is write(h); hence if the hash is done(h), the key
  // matches a required key, and the data value is subsequently read, then
  // that datum was correct at some point since reading the hash, even if the
  // hash has subsequently been changed to deleted(h).

  // The corresponding map contains keys(i) -> data(i) if hashes(i) contains a
  // done value, i.e. the map is
  // { state.keys(i) -> state.data(i) |  
  //     0 <= i < state.size, state.hashes(i) = done(h) (for some h) }
 
  // In addition, each entry can be flagged as having been copied into the new
  // hash table.  The "copied" flag can be sent once (but not from the write
  // state), and never reset.  Any thread seeing this flag set should help
  // with copying into the new table, and then restart its operation on the
  // new table.

  // This satisfies the normal DTI for open addressing hash tables: if entry i
  // contains a value with hash h, then all entries in [h%size .. i) (wrapping
  // round) are non-Empty.  state.count records the number of non-Empty
  // hashes.  newState contains no value with the copied flag set.

  // We use the lowest three bits of each hashes(i) to indicate the type of
  // value, and so limit the hash to 29 bits.  HashMask extracts these bits.
  protected val HashMask = 0x1FFFFFFF 
  private val Empty = 0 // So buckets are initialised to EMPTY
  private val WriteMask = 2<<30
  private def Write(h: Int) = h | WriteMask
  private val DoneMask = 3<<30
  private def Done(h: Int) = h | DoneMask
  private def isDone(h: Int) = (h & DoneMask) == DoneMask
  private val DelMask = 1<<30
  private def Deleted(h: Int) = h | DelMask
  private def isDeleted(h: Int) = (h & DoneMask) == DelMask
  private val CopiedMask = 1<<29
  private def isCopied(h: Int) = (h & CopiedMask) == CopiedMask
  private def copied(h: Int) = h | CopiedMask
  private val IgnoreCopyMask = HashMask+DoneMask
  private def ignoreCopy(h: Int) = h & IgnoreCopyMask

  override protected def hash(k: A) : Int =  improve(k.hashCode) & HashMask

  /** Wait for a random amount of time */
  private def pause = { } // Thread.sleep(0, scala.util.Random.nextInt(20))

  /** If key is not in the map, initialise it op; otherwise 
    * return the value it maps to */
  def getOrElseUpdate(key: A, op: => B) : B = {  
    val myState = state.get
    if(myState.count.get >= myState.threshold){ 
      resize(myState); getOrElseUpdate(key, op)
    }
    else{
      val h = hash(key); var done = false; var res = null.asInstanceOf[B]
      val myHashes = myState.hashes; val myKeys = myState.keys
      val mask = myState.size - 1; var index = h & mask
      // Try to insert the entry, starting from position index
      do{
	val h1 = myHashes.get(index)
	if(h1 == Empty){
	  if(myHashes.compareAndSet(index, Empty, Write(h))){
	    // We now have ownership of this slot
	    myKeys(index) = key; 
	    res = op; myState.data(index) = res  // Add the data
	    myHashes.set(index, Done(h))  // release the slot
	    myState.count.inc; done = true
	  } // if CAS fails, go round the loop and re-read this bucket
	}
	else if(h1 == Done(h) && myKeys(index) == key){
	  res = myState.data(index); done = true
	}
	else if(h1 == Deleted(h) && myKeys(index) == key){
	  if(myHashes.compareAndSet(index, h1, Write(h))){
	    // We now have ownership of this slot
	    res = op; myState.data(index) = res  // Add the data
	    myHashes.set(index, Done(h)); done = true // release the slot
	  } // if CAS fails, go round the loop and re-read this bucket
	}
	else if(isCopied(h1)){
	  // Resize is in progress; help with that, then try again
	  helpCopy(myState); res = getOrElseUpdate(key, op); done = true
	}
	else if(h1 != Write(h)){ // wrong key, maybe wrong hash
	  // (h1 == Done(h) || h1 == Deleted(h)) && myKeys(index) != key || 
	  // (h1 & HashMask) != h)
	  index = (index + 1) & mask
	}
	else // someone else writing to this slot; key might not match
	  pause // go round the loop and re-read hash
      } while(!done)
      res 
    }
  } 

  /** If key is not in the map, initialise it datum; otherwise 
    * return the value it maps to */
  // def getOrElseUpdateV(key: A, datum: B) : B 
  // = Profiler.time("getOrElseUpdateV"){  
  //   Profiler.count("getOrElseUpdateV")
  //   val myState = state.get
  //   if(myState.count.get >= myState.threshold){ 
  //     resize(myState); getOrElseUpdate(key, datum)
  //   }
  //   else{
  //     val h = hash(key); var done = false; var res = datum
  //     val myHashes = myState.hashes; val myKeys = myState.keys
  //     val mask = myState.size - 1; var index = h & mask
  //     // Try to insert the entry, starting from position index
  //     do{
  // 	val h1 = myHashes.get(index)
  // 	if(h1 == Empty){
  // 	  if(myHashes.compareAndSet(index, Empty, Write(h))){
  // 	    // We now have ownership of this slot
  // 	    myKeys(index) = key; 
  // 	    myState.data(index) = res  // Add the data
  // 	    myHashes.set(index, Done(h))  // release the slot
  // 	    myState.count.inc; done = true
  // 	  } // if CAS fails, go round the loop and re-read this bucket
  // 	} 
  // 	else if(h1 == Done(h) && myKeys(index) == key)
  // 	  return myState.data(index)
  // 	else if(h1 == Deleted(h) && myKeys(index) == key){
  // 	  if(myHashes.compareAndSet(index, h1, Write(h))){
  // 	    // We now have ownership of this slot
  // 	    myState.data(index) = res  // Add the data
  // 	    myHashes.set(index, Done(h)); done = true // release the slot
  // 	  } // if CAS fails, go round the loop and re-read this bucket
  // 	}
  // 	else if(isCopied(h1)){
  // 	  // Resize is in progress; help with that, then try again
  // 	  helpCopy(myState); res = getOrElseUpdate(key, datum); done = true
  // 	}
  //       else if(h1 != Write(h)){ // wrong key, maybe wrong hash
  // 	  assert((h1 == Done(h) || h1 == Deleted(h)) && myKeys(index) != key || 
  // 		 (h1 & HashMask) != h)
  // 	  index = (index + 1) & mask
  // 	}
  // 	else{ // someone else writing to this slot; key might not match
  // 	  assert(h1 == Write(h)); pause // go round the loop and re-read hash
  // 	}
  //     } while(!done)
  //     res 
  //   }
  // } 

  /** Get the value associated with key k, or default if the key is not in 
    * the map. */
  def getOrElse(key: A, default: => B): B = { 
    val h = hash(key); val myState = state.get; val myHashes = myState.hashes
    val mask = myState.size - 1; var index = h & mask
    var done = false; var res = null.asInstanceOf[B]
    // Try to find the entry, starting from position index
    do{
      // We ignore "copied" flags; the value read will still be valid
      val h1 = ignoreCopy(myHashes.get(index))
      if(h1 == Empty || h1 == Deleted(h) && myState.keys(index) == key){
	res = default; done = true
      }
      else if(h1 == Done(h) && myState.keys(index) == key){
	res = myState.data(index); done = true
      }
      // else if(isCopied(h1)){ 
      // // The following is an alternative strategy to ignoring "copied"
      // 	// flags.
      // 	helpCopy(myState); return getOrElse(key, default) 
      // }
      else if(h1 != Write(h)) index = (index + 1) & mask // wrong key
      else pause 
    } while(!done)
    res
    // If the read is from the current state, the linearization point is the
    // final point at which h1 is read, or (in the case of a successful get)
    // the point at which another thread writes the value returned, whichever
    // is later.  If the read is from an old state, then the linearization
    // point is the point of the "state.get", or the final point at which this
    // slot was updated (which was while the state was current), whichever is
    // later.
  }

  /** Add key -> data to the mapping, overwriting any previous value there. */
  def update(key: A, datum: B) : Unit = { 
    val myState = state.get
    if(myState.count.get >= myState.threshold){ 
      resize(myState); update(key, datum)
    }
    else{
      var done = false; val h = hash(key); 
      val myHashes = myState.hashes; val myKeys = myState.keys
      val mask = myState.size - 1; var index = h & mask
      // Try to insert the entry, starting from position index
      do{
	val h1 = myHashes.get(index)
	if(h1 == Empty){
	  if(myHashes.compareAndSet(index, Empty, Write(h))){
	    // We now have ownership of this slot
	    myKeys(index) = key; myState.data(index) = datum // Add the data
	    myHashes.set(index, Done(h))   // Release the slot
	    myState.count.inc; done = true 
	  }  // if the CAS fails, go round the loop and re-read this bucket
	}
	else if((h1 == Done(h) || h1 == Deleted(h)) && myKeys(index) == key){
	  if(myHashes.compareAndSet(index, h1, Write(h))){
	    myState.data(index) = datum                 // Write the data
            myHashes.set(index, Done(h)); done = true   // Release the slot
	  }
	  // if the CAS fails, go round the loop and re-read this bucket (it
	  // might be being copied).
	} // end of if((h1 == Done(h) || h1 == Deleted(h)) && keys(index)==key)
	else if(isCopied(h1)){
	  // Resize is in progress; help with that, then try again
	  helpCopy(myState); update(key, datum); done = true
	}
	else if(h1 != Write(h)) // wrong key, maybe wrong hash
	  index = (index + 1) & mask
	else // someone else writing to this slot; key might not match
	  pause // go round the loop and re-read hash
      } while(!done) // end of while(!done)
    }
  }

  /** Delete the value associated with key */
  def delete(key: A): Unit = { 
    var done = false; val h = hash(key); val myState = state.get
    val myHashes = myState.hashes; val myKeys = myState.keys
    val mask = myState.size - 1; var index = h & mask
    // Try to delete the entry, starting from position index
    do{
      val h1 = myHashes.get(index)
      if(h1 == Empty) done = true
      else if(h1 == Done(h) && myKeys(index) == key){
	if(myHashes.compareAndSet(index, h1, Deleted(h))) done = true
	// If the CAS fails, go round again
      }
      else if(h1 == Deleted(h) && myKeys(index) == key) done = true
      else if(isCopied(h1)){
	// Resize in progress; help with that.
	helpCopy(myState); return delete(key)
      }
      else if(h1 != Write(h)) index = (index + 1) & mask
      else pause
    } while(!done)

  }

  // ------------------------------------------------------------------
  // Operations to do with resizing. 

  /** Number of items in a single task of copying */
  private val BlockSize = 64

  /** Help with resizing of the hash table.  This should be called only 
    * after newState has been initialised. */
  private def helpCopy(oldState: State) : Unit = {
    val myNewState = newState.get
    assert(oldState != myNewState)
    // Repeatedly copy blocks of entries
    var blockStart = oldState.nextCopy.getAndAdd(BlockSize)
    while(blockStart < oldState.size){
      val blockEnd = (blockStart+BlockSize) min oldState.size
      var count = 0 // count of items copied
      // Copy block [blockStart..blockEnd) into myNewState
      for(i <- blockStart until blockEnd)
	if(copyItem(i, oldState, myNewState)) count += 1
      myNewState.count.add(count)
      oldState.copied.getAndAdd(BlockSize)
      blockStart = oldState.nextCopy.getAndAdd(BlockSize)
    }

    // Wait for other threads to finish their copying
    while(oldState.copied.get < oldState.size){ }
    // The data is now complete; try once to install the new state
    if(state.get == oldState) state.compareAndSet(oldState, myNewState)
    // If the CAS fails, another thread has installed it
  }

  /** Copy item from position i of oldState into newState
    * @return true if item inserted into newState. */
  private def copyItem(i: Int, oldState: State, newState: State) : Boolean = {
    val oldHashes = oldState.hashes; val newHashes = newState.hashes
    val newMask = newState.size - 1
    while(true){
      val h1 = oldHashes.get(i)
      if(isDone(h1) && oldHashes.compareAndSet(i, h1, copied(h1))){
	val h = h1 & HashMask; var j = h & newMask 
	// try to copy into position j
	while(true){
	  val h2 = newHashes.get(j)
	  if(h2 == Empty && newHashes.compareAndSet(j, Empty, Write(h))){
	    // We now have ownership of the new slot
	    newState.keys(j) = oldState.keys(i)
	    newState.data(j) = oldState.data(i)
	    newHashes.set(j, h1); return true		// Release the slot
	  }
	  // slot j is full or being filled; try next slot
	  j = (j+1) & newMask
	} // end of while(true)
	sys.error("unreachable")
      } // end of if(isDone(h1) && ..)
      else if((h1 == Empty || isDeleted(h1)) && 
	      oldHashes.compareAndSet(i, h1, copied(h1)))
	return false
      // else it's being written or a CAS failed; go round the loop
    } // end of while(true)
    sys.error("unreachable")
  }

  /** Start resizing of the state */
  private def resize(oldState: State) = {
    if(newState.get == oldState){
      // Try to claim the token for creating the new state
      if(!oldState.resizing.getAndSet(true)){
	// Create the new state
	val myNewState = new State(oldState.size*2)
	// Set the new state. 
	val ok = newState.compareAndSet(oldState, myNewState)
	assert(ok)
      }
      else while(newState.get == oldState) pause
    }
    helpCopy(oldState)
  }

  // Other operations not implemented
  def clear: Unit = ???


}
