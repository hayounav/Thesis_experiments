package ox.cads.collection

import scala.reflect.ClassTag

/** A class representing maps from A to B, using sharding and synchronized 
  * blocks.  The hashes are memoized, and also used to record deleted values.
  * @param shards the number of shards.
  * @param initSize the initial size of each shard; this must be a power of 2
  * shards and initSize should be coprime. 
  */
class ShardedMap[A: ClassTag, B: ClassTag](shards: Int, initSize: Int)
extends HashMap[A, B]
{
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

  // Arrays holding the keys, data and hashes.
  private val keys = Array.fill(shards)(new Array[A](initSize))
  private val data = Array.fill(shards)(new Array[B](initSize))
  private val hashes = Array.fill(shards)(new Array[Int](initSize))

  // We store the hashes in hashes, stealing two bits to store whether there
  // is a key stored or deleted.
  private val HashMask = 0x3FFFFFFF
  @inline private def hashMask(h: Int) = h & HashMask
  private val DelMask = 0x80000000; private val StoreMask = 0x40000000
  private def delete(h: Int) = hashMask(h) | DelMask
  private def deleted(h: Int) = (h & DelMask) == DelMask
  private def store(h: Int) = hashMask(h) | StoreMask
  private def stored(h: Int) = (h & StoreMask) == StoreMask
  private val Empty = 0

  // Together, these arrays represent the mapping 
  // {keys(i)(j) -> data(i)(j) | i <- [0..shards), j <- [0..keys(i).size),
  //                             stored(hashes(i)(j)) }.
  // Each hash value is negative if the value is deleted; otherwise it stores 
  // the hash of the corresponding key. 
  // A value with key k is stored in shard number sh = hash(k) >>> shardShift, 
  // in the first free position after hash(h) % keys(sh).size.
  // DTI: if hashes(i)(j) != Empty, it is store(h) or delete(h) where 
  // h = hash(keys(i)(j).
  // If key k appears in position shards(j)(i), then every location 
  // hashes(j)[hash(k)%(hashes(j).size) .. i) is non-Empty

  /** Locks for each shard */
  private val locks = Array.fill(shards)(new AnyRef)

  /** The number of non-Empty entries in each shard (including deleted items) */
  private val numEntries = new Array[Int](shards)
  // DTI: numEntries(sh) = # { i | hashes(sh)(i) != Empty }

  /** Maximum load factor times 1000 in each shard */
  private val maxLoadFactor = 600 // corresponds to 0.6

  /** The initial threshold for resizing */
  private val initThresh = initSize * maxLoadFactor / 1000

  /** The thresholds for resizing each shard */
  private val threshold = Array.fill(shards)(initThresh)

  /** Find the location in ks where a key k with hash h is or should be 
    * placed.  More precisely, finds the first location i from h%(hs.size) 
    * where hs(i) stores store(h), delete(h) or Empty and (in the former two 
    * cases) ks(i) = k. */
  @inline private def findPlace(
    ks: Array[A], hs: Array[Int], k: A, h: Int) : Int 
  = {
    val size = ks.size; val mask = size-1; val h1 = hashMask(h)
    var i = h & mask
    while(hs(i) != Empty && (hashMask(hs(i)) != h1 || ks(i) != k)){
      i = (i+1) & mask
    }
    // Either: (1) hs(i) == Empty, so this slot is empty; or (2)
    // hashMask(hs(i)) == hashMask(h) and ks(i) == k, so we've found k.
    i
  }

  /** Get the value associated with key k, or default if the key is not in 
    * the map. */
  def getOrElse(key: A, default: => B) : B = {
    val h = hash(key); val sh = h>>>shardShift 
    locks(sh).synchronized{
      val ks = keys(sh); val hs = hashes(sh); val i = findPlace(ks, hs, key, h)
      if(hs(i) == store(h)) data(sh)(i) else default
    }
  }

  /** If key is not in the map, initialise it op; otherwise 
    * return the value it maps to */
  def getOrElseUpdate(key: A, op: => B) : B = {
    val h = hash(key); val sh = h>>>shardShift
    locks(sh).synchronized{
      if(numEntries(sh) >= threshold(sh)) resize(sh)
      val ks = keys(sh); val hs = hashes(sh); val i = findPlace(ks, hs, key, h)
      if(hs(i) == Empty){ 
	numEntries(sh) += 1; ks(i) = key; hs(i) = store(h)
	val res = op; data(sh)(i) = res; res
      }
      else if(hs(i) == delete(h)){ 
	hs(i) = store(h); val res = op; data(sh)(i) = res; res
      }
      else data(sh)(i) 
    } // end of synchronised block
  }

  /** Add key -> data to the mapping, overwriting any previous value there. */
  def update(key: A, datum: B) = {
    val h = hash(key); val sh = h>>>shardShift
    locks(sh).synchronized{
      if(numEntries(sh) >= threshold(sh)) resize(sh)
      val ks = keys(sh); val hs = hashes(sh)
      val i = findPlace(ks, hs, key, h)
      if(hs(i) == Empty){ ks(i) = key; numEntries(sh) += 1 }
      hs(i) = store(h); data(sh)(i) = datum
    }
  }

  /** Delete value associated with key. */
  def delete(key: A) : Unit = {
    val h = hash(key); val sh = h>>>shardShift
    locks(sh).synchronized{
      val ks = keys(sh); val hs = hashes(sh); val i = findPlace(ks, hs, key, h)
      if(hs(i) == store(h)) hs(i) = delete(h)
	// Don't decrement numEntries here.
    }
  }

  /** Clear the mapping. */
  def clear = {
    for(i <- 0 until shards){
      keys(i) = new Array[A](initSize); data(i) = new Array[B](initSize)
      hashes(i) = new Array[Int](initSize)
      numEntries(i) = 0; threshold(i) = initThresh
    }
  }

  /** Resize shard sh.
    * Pre: the current thread has locks(sh) locked. */
  private def resize(sh: Int) = {
    val ks = keys(sh); val ds = data(sh); val hs = hashes(sh)
    val size = ks.size; val newSize = size*2
    val newKs = new Array[A](newSize); val newDs = new Array[B](newSize)
    val newHs = new Array[Int](newSize)
    // Copy nodes from ks, hs, ds to newKs, newHs, newDs
    var i = 0
    var count = 0 // number copied so far
    while(i < size){ 
      if(stored(hs(i))){
	val j = findPlace(newKs, newHs, ks(i), hashMask(hs(i)))
        newKs(j) = ks(i); newDs(j) = ds(i); newHs(j) = hs(i)
	count += 1
      }
      i += 1
    }
    // Set new table
    keys(sh) = newKs; data(sh) = newDs; hashes(sh) = newHs
    threshold(sh) = threshold(sh)*2; numEntries(sh) = count
  }
}
