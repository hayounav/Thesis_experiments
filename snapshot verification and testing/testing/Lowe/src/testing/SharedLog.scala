package ox.cads.testing

/** A log to be used by several threads, based on a shared array.
  * Note that logging involves a synchronization event, which will have an 
  * effect upon the caches of threads, thereby possibly missing Java Memory
  * Model-based errors. 
  * @tparam S the type of the sequential specification object. 
  * @tparam C the type of the concurrent object. 
  * @param invocs the number of invocations to be logged
  * @param concObj the concurrent object.
  * @param mkInvoke function to create an InvokeEvent.
  * @param mkReturn function to create a new ReturnEvent.
*/

class SharedLog[S,C](
  invocs: Int, concObj: C, 
  mkInvoke: GenericLog.MkInvokeType[S], mkReturn: GenericLog.MkReturnType)
extends GenericLog[S, C]{

  /** BoundedBuffer used to store Events. */
  private val inQueue = new ox.cads.collection.BoundedBuffer[Event](2*invocs)

  /** Internal GenericThreadLog object. */
  class SharedThreadLog(t: Int) extends GenericThreadLog[S,C]{
  /** Log that the thread performs an operation. 
    *
    * @tparam A the type of the result of the operation.
    * @tparam B the type of the return of the operation on the sequential
    * object; this will be A for a tester based on an undoable sequential 
    * object, and (A,S) for a tester based on an immutable sequential object.
    *
    * @param concOp the operation on the concurrent datatype.
    * @param msg a string describing the operation, used in debugging output; 
    * semantically different operations should use different strings.  
    * @param seqOp the corresponding operation on the sequential datatype. */
    def log[A,B](concOp: C => A, msg: String, seqOp: S => B) = {
      // log invocation
      val e = mkInvoke(t,msg,seqOp); inQueue.add(e)
      // perform operation
      val result = concOp(concObj)
      // log return
      val e1 = mkReturn(t, result); inQueue.add(e1); e.ret = e1
    }
  }

  /** Get a GenericThreadLog object for thread t. */
  def apply(t: Int) = new SharedThreadLog(t)

  /** Get the contents of the log */
  def getLog : Array[Event] = inQueue.getAll

}
  
