package ox.cads.testing

/** A log to be used by a single thread, based on timestamping.
  * @tparam S the type of the sequential datatype. 
  * @tparam C the type of the concurrent datatype. 
  * @param t the identity of the thread.
  * @param iters the number of operations performed by this worker. 
  * @param concObj the concurrent object shared between workers. 
  * @param mkInvoke function to create an InvokeEvent.
  * @param mkReturn function to create a new ReturnEvent.  */ 

class TSThreadLog[S,C](
  t: Int, iters: Int, concObj: C,
  mkInvoke: GenericLog.MkInvokeType[S], mkReturn: GenericLog.MkReturnType)
extends GenericThreadLog[S,C]
{
  /** Array holding the events. */
  private val events = new Array[TS[Event]](2*iters)
		 
  /** Index of next free slot in events. */
  private var index = 0

  /** Log that thread t performs operation described by msg.
    *
    * @tparam A the type of the result of the operation.
    * @tparam B the type of the return of the operation on the sequential
    * object; this will be A for a tester based on an undoable sequential 
    * object, and (A,S) for a tester based on an immutable sequential object.
    *
    * @param concOp the operation on the concurrent datatype.
    * @param seqOp  the corresponding operation on the sequential datatype. */
  def log[A,B](concOp: C => A, msg: String, seqOp: S => B) = {
    // log invocation
    val e = mkInvoke(t, msg, seqOp) 
    events(index) = new TS(e); index += 1
    // perform operation
    val result = concOp(concObj)
    // log return
    val e1 = mkReturn(t, result) 
    events(index) = new TS(e1); index += 1; e.ret = e1
  }

  /** Get the log. */
  protected[testing] def get : Array[TS[Event]] = events

}


