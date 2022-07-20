package ox.cads.collection

/** A trait defining the interface of the hash sets. */
trait HashSet[A]{
    /** Improve the quality of a hash.
    * Code taken from the code for java.util.concurrent.ConcurrentHashMap */
  @inline protected final def improve(hcode: Int) = {
    var h = hcode
    // Spread bits to regularize both segment and index locations,
    // using variant of single-word Wang/Jenkins hash.
    h += (h <<  15) ^ 0xffffcd7d; h ^= (h >>> 10); h += (h <<   3)
    h ^= (h >>>  6); h += (h <<   2) + (h << 14); h ^ (h >>> 16)
  }

  /** A hash value guaranteed to be non-negative */
  @inline protected def hash(k: A) : Int = improve(k.hashCode)

  /** Put key into the set.  Return true if it is a new value */
  def putIfAbsent(key: A) : Boolean
}
