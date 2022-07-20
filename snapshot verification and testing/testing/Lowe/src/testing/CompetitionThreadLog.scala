package ox.cads.testing

import scala.collection.mutable.Undoable

/** A log to be used by a single thread for a competition tester using an 
  * immutable and an undoable datatype.  
  * @tparam S1 the first sequential specification datatype. 
  * @tparam S2 the second sequential specification datatype. 
  * @tparam C the concurent datatype. */
abstract class CompetitionThreadLog[S1, S2, C]{

  /** Log that this thread performs operation described by msg.
    * @tparam A the type of the result of the operation.
    * @param concOp the operation on the concurrent datatype.
    * @param seqOp1 the corresponding operation on the first 
    * sequential datatype.
    * @param seqOp2 the corresponding operation on the second 
    * sequential datatype.*/
  def log[A](
    concOp: C => A, msg: String, seqOp1: S1 => Any, seqOp2: S2 => Any) : Unit
}
