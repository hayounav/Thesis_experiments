package ox.cads.collection

/** A trait for total stacks. */
trait TotalStack[T]{
  /** Push value onto the stack. */
  def push(value: T) : Unit

  /** Pop a value off the stack and return it, or return None if the stack
    * is empty. */
  def pop : Option[T]
}
