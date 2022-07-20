package ox.cads.collection

/** A trait representing a generic pool. */
trait Pool[T]{
  /** put x into the pool */
  def add(x: T) : Unit

  /** Get a value from the pool.
    * @return Some(x) where x is the value obtained, or None if the pool is
    * empty. */
  def get : Option[T]
}
