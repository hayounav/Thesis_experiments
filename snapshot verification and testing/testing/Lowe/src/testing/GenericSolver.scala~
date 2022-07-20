package ox.cads.testing

/** An abstract class for generic linearizability algorithms. 
  * @tparam S the type of sequential specification objects. */

abstract class GenericSolver[S, E <: Event] extends Solver[E]{
  /** Make an invocation event. */
  protected[testing] 
  //def mkInvoke(t: Int, msg: String, seqOp: S => Any) : InvokeEvent[S,Any,Any]
  val mkInvoke : GenericLog.MkInvokeType[S]

  /** Make a return event. */
  // protected[testing] def mkReturn(t: Int, result: Any) : ReturnEvent[Any]
  protected[testing] val mkReturn : GenericLog.MkReturnType

  // The mkInvoke and mkReturn functions are defined as vals rather than defs,
  // because this seems necessary to reliably find errors that depend upon
  // Java Memory Model issues.  I think this is because with defs, every
  // logging action would involve a call upon this object, whereas with vals,
  // the calls are upon the function objects that are produced.  The former seems to create more memory 
}
