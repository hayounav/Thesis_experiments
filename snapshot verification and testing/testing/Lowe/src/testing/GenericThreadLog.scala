package ox.cads.testing

/** A log to be used by a single thread. 
  * @tparam S the type of the sequential specification object. 
  * @tparam C the type of the concurrent object. */

abstract class GenericThreadLog[S, C]{

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
  def log[A,B](concOp: C => A, msg: String, seqOp: S => B)

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
  def apply[A,B](concOp: C => A, msg: String, seqOp: S => B) = 
    log(concOp, msg, seqOp)

}
