package ox.cads.collection
import java.util.concurrent.atomic.AtomicIntegerArray
import scala.reflect.ClassTag

/** An implementation of a HashMap based upon "Boosting Multi-Core
  * Reachability Performance with Shared Hash Tables", by Alfons Laarman, 
  * Jaco van de Pol and Michael Weber, extended to allow deletion.   
  * @param size the initial size of each shard; this must be a power of 2
  */
class LvdPWMap[A: ClassTag, B: ClassTag](size: Int) extends HashMap[A, B]{
  def this() = this(256)

  // Check size is a power of 2
  private var i = size
  while(i > 2){ assert(i%2 == 0); i = i/2 }

  private val Mask = size-1 // h & Mask = h % size

  /** Main arrays for the hash table */
  private val hashes = new AtomicIntegerArray(size)
  private val keys = new Array[A](size)
  private val data = new Array[B](size)

  // Abstractly, each hashes(i) contains one of the following values: Empty
  // (indicating an empty slot), write(h) indicating that a value with hash h
  // is currently being written, done(h) indicating that a value with hash h
  // has been inserted, or deleted(h) indicating that a value with hash h has
  // been deleted.  The state transitions in the sequence Empty -> write(h) ->
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
  // { keys(i) -> data(i) | 0 <= i < size, hashes(i) = done(h) (for some h) }

  // We use the lowest two bits to indicate the type of value, and so limit
  // the hash to 30 bits.  HashMask extracts these bits.
  protected val HashMask = 0x3FFFFFFF 
  private def hashMask(h: Int) = h // & HashMask
  private val Empty = 0 // So buckets are initialised to EMPTY
  private val WriteMask = 2<<30
  private def Write(h: Int) = hashMask(h) | WriteMask
  private val DoneMask = 3<<30
  private def Done(h: Int) = hashMask(h) | DoneMask
  // private def isDone(h: Int) = (h & DoneMask) == DoneMask
  private val DelMask = 1<<30
  private def Deleted(h: Int) = hashMask(h) | DelMask

  @inline override protected def hash(k: A) : Int = 
    improve(k.hashCode) & HashMask

  /** Wait for a random amount of time */
  private def pause = { } // Thread.sleep(0, scala.util.Random.nextInt(20))

  /** If key is not in the map, initialise it op; otherwise 
    * return the value it maps to */
  def getOrElseUpdate(key: A, op: => B) : B = {   
    val h = hash(key); var index = h & Mask
    var res = null.asInstanceOf[B]; var done = false
    // Try to insert the entry, starting from position index
    do{
      val h1 = hashes.get(index)
      if(h1 == Empty){
	if(hashes.compareAndSet(index, Empty, Write(h))){
	  // We now have ownership of this slot
	  keys(index) = key; res = op; data(index) = res  // Add the data
	  hashes.set(index, Done(h)); done = true // release the slot
	} // if CAS fails, go round the loop and re-read this bucket
      }
      else if(h1 == Done(h) && keys(index) == key){
	res = data(index); done = true
      }
      else if(h1 == Deleted(h) && keys(index) == key){
	if(hashes.compareAndSet(index, h1, Write(h))){
	  // We now have ownership of this slot
	  res = op; data(index) = res  // Add the data
	  hashes.set(index, Done(h)); done = true // release the slot
	} // if CAS fails, go round the loop and re-read this bucket
      }
      else if(h1 != Write(h)) // wrong key, maybe wrong hash
	index = (index + 1) & Mask
      else // someone else writing to this slot; key might not match
	pause // go round the loop and re-read hash
    } while(!done)
    res
    // sys.error("How did we get here?")
  } 

  /** Get the value associated with key k, or default if the key is not in 
    * the map. */
  def getOrElse(key: A, default: => B): B = {
    val h = hash(key); var index = h & Mask
    var done = false; var res = null.asInstanceOf[B]
    // Try to insert the entry, starting from position index
    do{
      val h1 = hashes.get(index)
      if(h1 == Empty || h1 == Deleted(h) && keys(index) == key){ 
	res = default; done = true 
      }
      else if(h1 == Done(h) && keys(index) == key){ 
	res = data(index); done = true 
      }
      else if(h1 != Write(h)) index = (index + 1) & Mask // wrong key
      else pause 
    } while(!done)
    res
  }

  /** Add key -> data to the mapping, overwriting any previous value there. */
  def update(key: A, datum: B) = {
    var done = false; val h = hash(key); var index = h & Mask
    // Try to insert the entry, starting from position index
    do{
      val h1 = hashes.get(index)
      if(h1 == Empty){
	if(hashes.compareAndSet(index, Empty, Write(h))){
	  // We now have ownership of this slot
	  keys(index) = key; data(index) = datum   // Add the data
	  hashes.set(index, Done(h)); done = true  // Release the slot
	}  // if the CAS fails, go round the loop and re-read this bucket
      }
      else if((h1 == Done(h) || h1 == Deleted(h)) && keys(index) == key){
	if(hashes.compareAndSet(index, h1, Write(h))){
	  data(index) = datum                     // Write the data
          hashes.set(index, Done(h)); done = true // Release the slot
	}
	// if the CAS fails, another thread is writing to the same key; go
	// round the loop again (if the other operation is a getOrElseUpdate
	// on a previously deleted slot, we cannot just pretend that this
	// operation happened before that one).
      } // end of if((h1 == Done(h) || h1 == Deleted(h)) && keys(index) == key)
      else if(h1 != Write(h)) // wrong key, maybe wrong hash
	index = (index + 1) & Mask
      else // someone else writing to this slot; key might not match
	pause // go round the loop and re-read hash
    } while(!done) // end of while(!done)
  }

  /** Delete the value associated with key */
  def delete(key: A): Unit = {
    var done = false; val h = hash(key); var index = h & Mask
    // Try to delete the entry, starting from position index
    do{
      val h1 = hashes.get(index)
      if(h1 == Empty) done = true
      else if(h1 == Done(h) && keys(index) == key){
	if(hashes.compareAndSet(index, h1, Deleted(h))) done = true
	// If CAS fails, need to re-try: another thread is doing an update. 
      }
      else if(h1 == Deleted(h) && keys(index) == key) done = true
      else if(h1 != Write(h)) index = (index + 1) & Mask
      else pause
    } while(!done)

  }

  // Other operations not implemented
  def clear: Unit = ???


}
