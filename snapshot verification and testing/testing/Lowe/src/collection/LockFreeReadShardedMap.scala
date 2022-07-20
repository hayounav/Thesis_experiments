/** A class representing maps from A to B, using sharding and synchronized 
  * blocks.  The value null is used to record a deleted item.  
  * @param shards the number of shards.
  * @param initSize the initial size of each shard; this must be a power of 2
  * shards and initSize should be coprime. 
  */
package ox.cads.collection
import java.util.concurrent.atomic.{AtomicReference,AtomicReferenceArray}
import scala.reflect.ClassTag
// import ox.cads.util.VolatileArray

class LockFreeReadShardedMap[A: ClassTag, B >: Null]
    (shards: Int, initSize: Int)
    (implicit m: ClassTag[B])
extends HashMap[A, B]
{
  def this()(implicit m: ClassTag[B]) = this(256, 16)

  // Check initSize and shards are powers of 2; calculate the shift for
  // picking shards.
  private var s = initSize
  while(s > 2){ assert(s%2 == 0); s = s/2 }

  private var logShards = 0; s = shards  // Inv: s * 2^logShards = shards
  while(s>1){ assert(s%2 == 0); s = s/2; logShards += 1 }
  // We will store a value with hash h in shard s >>> shardShift, i.e. the top
  // logShards bits
  private val shardShift = 32 - logShards

  // Each shard is represented by an object of the following type.
  private class Shard(
      val keys: Array[A], 
      val data: AtomicReferenceArray[B],
      val hashes: Array[Int]){
    def this() = 
      this(new Array[A](initSize), 
	   new AtomicReferenceArray[B](initSize),
	   new Array[Int](initSize))
  }

  private val state = new AtomicReferenceArray[Shard](shards)
  for(i <- 0 until shards) state.set(i, new Shard)
  @inline private def getState(sh: Int) = state.get(sh)
  @inline private def setState(i: Int, shard: Shard) = state.set(i, shard) 
  
  // We store the hashes in hashes, stealing one bit to indicate that the
  // bucket is occupied.
  private val HashMask = 0x7FFFFFFF
  // The hash function returns a 32 bit hash.  The top logShards bits are
  // considered in calculating the shard to use.  The top bit is ignored in
  // the stored hash.
  private val StoreMask = 0x80000000
  private def full(h: Int) = h | StoreMask
  private def filled(h: Int) = (h & StoreMask) == StoreMask
  private val Empty = 0

  // We set data(i) to null to indicate a deleted item.  Together, these
  // arrays represent the mapping 
  // {st.keys(i) -> st.data(i) | 
  //    st <- state.map(get), i <- [0..st.keys.size), 
  //    filled(st.hash(i)) && st.data(i) != null }
  // A value with key k is stored in shard number sh = hash(h) >>> shardShift, 
  // in the first free position after hash(h) % getState(sh).keys(sh).size.

  /** Locks for each shard */
  private val locks = Array.fill(shards)(new AnyRef)

  /** The number of non-Empty entries in each shard (including deleted 
    * items) */
  private val numEntries = new Array[Int](shards)
  // DTI: numEntries(sh) = # { i | hashes(sh)(i) != Empty }

  /** Maximum load factor times 1000 in each shard */
  private val maxLoadFactor = 600 // corresponds to 0.6

  /** The initial threshold for resizing */
  private val initThresh = initSize * maxLoadFactor / 1000

  /** The thresholds for resizing each shard */
  private val threshold = Array.fill(shards)(initThresh)

  /** Find the location in ks where a key k with hash h is or should be 
    * placed.  More precisely, finds the first location i from h%(ks.size) 
    * where hs(i) = Empty or ks(i) = k. */
  @inline private def findPlace(ks: Array[A], hs: Array[Int], k: A, h: Int) 
  : Int = {
    val size = ks.size; val mask = size-1; var i = h & mask; val h1 = full(h)
    while(hs(i) != Empty && (hs(i) != h1 || ks(i) != k)){
      i = (i+1) & mask
    }
    // Either: (1) hs(i) == Empty, so this slot is empty; or
    // hs(i) == full(h) and ks(i) == k, so we've found k.
    i
  }
      
  /** Get the value associated with key k, or default if the key is not in 
    * the map. */
  def getOrElse(key: A, default: => B) : B = {
    val h = hash(key); val sh = h>>>shardShift
    val myState = getState(sh) // linearization point if state changes in a
			       // way that doesn't propagate to this thread
    val ks = myState.keys; val hs = myState.hashes
    // inline findPlace, as we want to know if key found
    val size = ks.size; val mask = size-1; val h1 = full(h)
    var i = h & mask; var found = false // was key found?
    while(hs(i) != Empty && {found = hs(i) == h1 && ks(i) == key ; !found}){
      i = (i+1) & mask
    }
    if(found){
      val d = myState.data.get(i) // linearization point
      if(d == null) default else d
    }
    else default // the read of hs(i) = Empty is linearization point
  }

  /** If k is not in the map, initialise it to the result of op; otherwise 
    * return the value it maps to. */
  def getOrElseUpdate(key: A, op: => B) : B = {
    // Try to do a lock-free read
    val h = hash(key); val sh = h>>>shardShift; var d = null.asInstanceOf[B]
    val myState = getState(sh) // linearization point if state changes in a
			       // way that doesn't propagate to this thread
    val ks = myState.keys; val hs = myState.hashes
    // inline findPlace, as we want to know if key found
    val size = ks.size; val mask = size-1; val h1 = full(h)
    var i = h & mask; var found = false // was key found?
    while(hs(i) != Empty && {found = hs(i) == h1 && ks(i) == key ; !found}){
      i = (i+1) & mask
    }
    if(found && { d = myState.data.get(i); d != null }) d
    // Else get the lock and do a write
    else locks(sh).synchronized{
      if(numEntries(sh) >= threshold(sh)){ // resizing necessary
	resize(sh); getOrElseUpdate(key, op) 
      }
      else if(getState(sh) != myState) // state has changed; restart
	getOrElseUpdate(key, op) 
      else{ // Need to recheck slot
	found = false
	while(hs(i) != Empty && {found = hs(i) == h1 && ks(i) == key; !found}){
	  i = (i+1) & mask
	}
	if(!found){ // new key
	  ks(i) = key; val res = op; myState.data.set(i, res); hs(i) = full(h)
	  numEntries(sh) += 1; res 
	}
	else{  // existing key
	  val d = myState.data.get(i)
	  if(d != null) d
	  else{ // deleted key
	    val res = op; myState.data.set(i, res); res
	  }
	}
      }
    } // end of synchronized block
  }
  //     else if(hs(i) == Empty){ // new key
  // 	ks(i) = key; val res = op; myState.data.set(i, res); hs(i) = full(h)
  // 	numEntries(sh) += 1; res 
  //     }
  //     else if(hs(i) == h1 && ks(i) == key && myState.data.get(i) == null){
  // 	// deleted key
  // 	val res = op; myState.data.set(i, res); res
  //     }
  //     else{  // this slot has been written to, could optimise here
  // 	getOrElseUpdate(key, op)
  //     }
  //   }
  // }

  // 	val ks = myState.keys; val hs = myState.hashes
  // 	val i = findPlace(ks, hs, key, h)
  //     if(hs(i) != Empty){
  // 	// assert(ks(i) == key) // TODO: remove
  // 	val d = myState.data.get(i)
  // 	if(d != null) d
  // 	else{ // Key is deleted
  // 	  val res = op; myState.data.set(i, res); res
  // 	}
  //     }
  //     else{ // new key.  Write to hs(i) is linearization point
  // 	ks(i) = key; val res = op; myState.data.set(i, res); hs(i) = full(h)
  // 	numEntries(sh) += 1; res 
  //     }
  //   } // end of synchronised block
  // }

  /** Add key -> data to the mapping, overwriting any previous value there. */
  def update(key: A, datum: B) = {
    val h = hash(key); val sh = h>>>shardShift
    locks(sh).synchronized{
      if(numEntries(sh) >= threshold(sh)) resize(sh)
      val myState = getState(sh)
      val ks = myState.keys; val hs = myState.hashes
      val i = findPlace(ks, hs, key, h)
      if(hs(i) == Empty){ 
	ks(i) = key; myState.data.set(i, datum) 
	hs(i) = full(h); numEntries(sh) += 1 // Linearisation point
      }
      else{
	// assert(hs(i) == full(h) && ks(i) == key); 
	myState.data.set(i, datum)
      }
    }
  }

  /** Delete value associated with key */
  def delete(key: A) : Unit = {
    val h = hash(key); val sh = h>>>shardShift
    locks(sh).synchronized{
      val myState = getState(sh);
      val ks = myState.keys; val hs = myState.hashes
      val i = findPlace(ks, hs, key, h)
      if(hs(i) != Empty){
	// assert(ks(i) == key) // TODO: delete
	myState.data.set(i, null)
      }
    }
  }

  /** Clear the mapping. */
  def clear : Unit = {
    for(i <- 0 until shards){
      setState(i, new Shard); numEntries(i) = 0; threshold(i) = initThresh
    }
  }

  /** Clear the mapping, but keep tables the same size. */
  // def clear1 : Unit = {
  //   for(i <- 0 until shards){
  //     for(j <- 0 until keys(i).size){
  // 	keys(i)(j) = emptyKey; data(i)(j) = null
  //     }
  //     numEntries(i) = 0
  //   }
  // }
    
  /** Resize shard sh.
    * Pre: the current thread has locks(sh) locked. */
  private def resize(sh: Int) = {
    val myState = getState(sh); val ks = myState.keys; val ds = myState.data
    val hs = myState.hashes; val size = ks.size; val newSize = size*2
    val newKs = new Array[A](newSize)
    val newDs = new AtomicReferenceArray[B](newSize)
    val newHs = new Array[Int](newSize)
    // Copy nodes from ks, ds, hs to newKs, newDs, newHs
    var i = 0; var count = 0 // number copied so far
    while(i < size){ 
      val h = hs(i); 
      if(filled(h)){
	val d = ds.get(i)
	if(d != null){
	  val k = ks(i); val j = findPlace(newKs, newHs, k, h)
          newKs(j) = k; newDs.set(j, d); newHs(j) = h; count += 1
	} // else deleted item, don't copy
      } // end of if(filled(h))
      i += 1
    }
    // Set new table
    val newShard = new Shard(newKs, newDs, newHs)
    setState(sh, newShard)
    threshold(sh) = threshold(sh)*2; numEntries(sh) = count
  }

}
