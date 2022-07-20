package ox.cads.collection

/** An abstract Set trait.
  * Represents a set S with elements taken from T. */
trait Set[T]{
  /* state S : \power T */

  /** Add x to S.
    * post: S = S_0 union {x} && returns x \not\in S_0 */
  def add(x: T) : Boolean

  /** Remove x from S.
    * post S = S_0 \ {x} && returns x \in S_0 */
  def remove(x: T) : Boolean

  /** Test if x in S.
    * post S = S_0 && returns x \in S_0 */
  def contains(x: T) : Boolean
}
