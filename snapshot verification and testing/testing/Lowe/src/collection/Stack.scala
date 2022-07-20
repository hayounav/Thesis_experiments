package ox.cads.collection

/** A trait for stacks. */
trait Stack[T]{
  /** Push value onto the stack. */
  def push(value: T) : Unit

  /** Pop a value off the stack and return it. */
  def pop : T
}
