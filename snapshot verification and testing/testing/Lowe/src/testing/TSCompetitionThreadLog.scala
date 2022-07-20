package ox.cads.testing

/** A log to be used by a single thread for a competition tester using two 
  * generic solvers.
  * @tparam S1 the first sequential specification datatype. 
  * @tparam S2 the second sequential specification datatype. 
  * @tparam C the concurrent datatype.
  *
  * @param t the identity of this worker.
  * @param iters the number of invocations by each thread.
  * @param concObj the concurrent object.
  * @param mkInvoke1 function to create the InvokeEvent corresponding to S1.
  * @param mkReturn1 function to create the ReturnEvent corresponding to S1.
  * @param mkInvoke2 function to create the InvokeEvent corresponding to S2.
  * @param mkReturn2 function to create the ReturnEvent corresponding to S2. */
class TSCompetitionThreadLog[S1, S2, C](
  t: Int, iters: Int, concObj: C,
  mkInvoke1: GenericLog.MkInvokeType[S1],
  mkInvoke2: GenericLog.MkInvokeType[S2],
  mkReturn1: GenericLog.MkReturnType, mkReturn2: GenericLog.MkReturnType)
extends CompetitionThreadLog[S1, S2, C]{

  /** Array holding the events. */
  private val events = new Array[TS[(Event,Event)]](2*iters)
		 
  /** Index of next free slot in events. */
  private var index = 0

  /** Log that the thread performs an operation. 
    * @tparam A the type of the result of the operation.
    * @param concOp the operation on the concurrent datatype.
    * @param msg a string describing the operation, used in debugging output; 
    * semantically different operations should use different strings.  
    * @param seqOp1 the corresponding operation on the first
    * sequential datatype. 
    * @param seqOp2 the corresponding operation on the second
    * sequential datatype. */  
  def log[A](
    concOp: C => A, msg: String, seqOp1: S1 => Any, seqOp2: S2 => Any) 
  = {
    // log invocation
    val inv1 = mkInvoke1(t, msg, seqOp1); val inv2 = mkInvoke2(t, msg, seqOp2)
    events(index) = new TS((inv1, inv2)); index += 1
    // perform operation
    val result = concOp(concObj)
    // log return
    val ret1 = mkReturn1(t, result); val ret2 = mkReturn2(t, result)
    events(index) = new TS((ret1, ret2)); index += 1
    inv1.ret = ret1; inv2.ret = ret2
  }

  /** Get the log. */
  protected[testing] def get : Array[TS[(Event,Event)]] = events

}
