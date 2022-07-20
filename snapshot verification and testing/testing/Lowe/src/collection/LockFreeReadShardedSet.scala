package ox.cads.collection

import java.util.concurrent.atomic.AtomicReferenceArray
import scala.reflect.ClassTag

/** A class representing a set of As, using sharding and synchronized 
  * blocks, with lock-free reads.  The hashes are memoized.
  * @param shards the number of shards.
  * @param initSize the initial size of each shard; this must be a power of 2
  * shards and initSize should be coprime. 
  */
class LockFreeReadShardedSet[A: ClassTag](shards: Int, initSize: Int) 
extends HashSet[A]{
  def this() = this(256, 16)

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
  private class Shard(val keys: Array[A], val hashes: Array[Int]){
    def this() = 
      this(new Array[A](initSize), new Array[Int](initSize))
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

  // This represents the set
  // {st.keys(i) | 
  //    st <- state.map(get), i <- [0..st.keys.size), filled(st.hash(i)) }
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

  /** If x is not in the map, add it; return true if it is a new value */
  def putIfAbsent(x: A) : Boolean = {
    // Try to do a lock-free read
    val h = hash(x); val sh = h>>>shardShift
    val myState = getState(sh) // linearization point if state changes in a
                               // way that doesn't propagate to this thread
    val ks = myState.keys; val hs = myState.hashes
    // inline findPlace, as we want to know if key found
    val size = ks.size; val mask = size-1; val h1 = full(h)
    var i = h & mask; var found = false // was key found?
    while(hs(i) != Empty && {found = hs(i) == h1 && ks(i) == x ; !found}){
      i = (i+1) & mask
    }
    if(found) false
    // Else get the lock and do a write
    else locks(sh).synchronized{
      if(numEntries(sh) >= threshold(sh)){ // resizing necessary
        resize(sh); putIfAbsent(x)
      }
      else if(getState(sh) != myState) // state has changed; restart
        putIfAbsent(x)
      else{ // Need to recheck slot
        found = false
        while(hs(i) != Empty && {found = hs(i) == h1 && ks(i) == x; !found}){
          i = (i+1) & mask
        }
        if(!found){ // new key
          ks(i) = x; hs(i) = full(h); numEntries(sh) += 1; true
        }
        else false  // existing key, added since first search
      }
    } // end of synchronized block
  }

  /** Resize shard sh.
    * Pre: the current thread has locks(sh) locked. */
  private def resize(sh: Int) = {
    val myState = getState(sh); val ks = myState.keys
    val hs = myState.hashes; val size = ks.size; val newSize = size*2
    val newKs = new Array[A](newSize)
    val newHs = new Array[Int](newSize)
    // Copy nodes from ks, ds, hs to newKs, newDs, newHs
    var i = 0; var count = 0 // number copied so far; ??? needed ???
    while(i < size){ 
      val h = hs(i); 
      if(filled(h)){
        val k = ks(i); val j = findPlace(newKs, newHs, k, h)
        newKs(j) = k; newHs(j) = h; count += 1
      } // end of if(filled(h))
      i += 1
    }
    // Set new table
    val newShard = new Shard(newKs, newHs); setState(sh, newShard)
    threshold(sh) = threshold(sh)*2; numEntries(sh) = count
  }



}
