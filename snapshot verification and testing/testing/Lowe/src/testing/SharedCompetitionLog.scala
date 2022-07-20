package ox.cads.testing
import scala.collection.mutable.Undoable

/** A shared log used by a competition tester.   
  *
  * @tparam S1 the first sequential specification datatype. 
  * @tparam S2 the second sequential specification datatype. 
  * @tparam C the concurent datatype.
  *
  * @param invocs the total number of invocations.
  * @param concObj the concurrent object.
  * @param mkInvoke1 function to create the InvokeEvent corresponding to S1.
  * @param mkReturn1 function to create the ReturnEvent corresponding to S1.
  * @param mkInvoke2 function to create the InvokeEvent corresponding to S2.
  * @param mkReturn2 function to create the ReturnEvent corresponding to S2.
  */
class SharedCompetitionLog[S1, S2, C](
  invocs: Int, concObj: C, 
  mkInvoke1: GenericLog.MkInvokeType[S1], mkReturn1: GenericLog.MkReturnType,
  mkInvoke2: GenericLog.MkInvokeType[S2], mkReturn2: GenericLog.MkReturnType
)
extends CompetitionLog[S1, S2, C]{

  /** BoundedBuffer used to store Events. */
  private val inQueue = 
    new ox.cads.collection.BoundedBuffer[(Event,Event)](2*invocs)

  /** Internal CompetitionThreadLog object. */
  class SharedThreadLog(t: Int) extends CompetitionThreadLog[S1, S2, C]{

    /** Log that the thread performs an operation. 
    * @tparam A the type of the result of the operation.
    * @param concOp the operation on the concurrent datatype.
    * @param msg a string describing the operation, used in debugging output; 
    * semantically different operations should use different strings.  
    * @param seqOp1 the corresponding operation on the first 
    * sequential datatype. 
    * @param seqOp2 the corresponding operation on the second
    * sequential datatype. */
    def log[A](concOp: C => A, msg: String, 
	       seqOp1: S1 => Any, seqOp2: S2 => Any) 
    = {
      // Create InvokeEvents, and add to log
      val inv1 = mkInvoke1(t, msg, seqOp1);
      val inv2 = mkInvoke2(t, msg, seqOp2)
      inQueue.add((inv1, inv2))
      // run the operation
      val result = concOp(concObj)
      // Create return events, and add to log. 
      val ret1 = mkReturn1(t, result); val ret2 = mkReturn2(t, result)
      inQueue.add((ret1, ret2))
      inv1.ret = ret1; inv2.ret = ret2
    }
  }

  /** Get a GenericThreadLog object for thread t. */
  def apply(t: Int) = new SharedThreadLog(t)

  /** Get the contents of the log */
  def getLog : Array[(Event, Event)] = inQueue.getAll

}
