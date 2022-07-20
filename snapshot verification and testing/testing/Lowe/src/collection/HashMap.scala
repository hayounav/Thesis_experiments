package ox.cads.collection

/** A trait defining the interface of the various hash maps. */
trait HashMap[A, B]{

  /** Improve the quality of a hash.
    * Code taken from the code for java.util.concurrent.ConcurrentHashMap */
  @inline protected final def improve(hcode: Int) = {
    var h = hcode
    // Spread bits to regularize both segment and index locations,
    // using variant of single-word Wang/Jenkins hash.
    h += (h <<  15) ^ 0xffffcd7d; h ^= (h >>> 10); h += (h <<   3)
    h ^= (h >>>  6); h += (h <<   2) + (h << 14); h ^ (h >>> 16)
  }

  /** A hash value. */
  @inline protected def hash(k: A) : Int = improve(k.hashCode)

  /** Get the value associated with key k, or default if the key is not in 
    * the map. */
  def getOrElse(key: A, default: => B) : B

  /** If k is not in the map, initialise it to the result of op; otherwise 
    * return the value it maps to. */
  def getOrElseUpdate(key: A, op: => B) : B
  
  /** Add key -> data to the mapping, overwriting any previous value there. */
  def update(key: A, datum: B)

  /** Delete value associated with key */
  def delete(key: A) : Unit

  /** Clear the mapping. */
  def clear : Unit
}
