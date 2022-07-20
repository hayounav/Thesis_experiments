package ox.cads.testing

/** A configuration, with undoing.  Used in the JITLinUndoTester. */

class UndoConfig[S <: scala.collection.mutable.Undoable](seqObj: S, p: Int){
  /** The type of states of threads. */
  abstract class ThreadState

  /** A thread has invoked an operation described by msg, corresponding
    * to operation op on the sequential object; this operation has not
    * yet been linearized. */
  case class Pending[S,A](msg: String, op: S => A, result: A) 
  extends ThreadState{
    override def toString = msg
  }

  /** A thread has had an operation linearized, producing res, but this has
    * not yet returned. */
  case class Ret(res: Any) extends ThreadState

  /** A thread is not currently in any operation call */
  case object Out extends ThreadState

  /** The state of threads */
  private val tStates = Array.fill[ThreadState](p)(Out)

  /** Add that op is invoked by t. */
  def invoke[A](t: Int, msg: String, op: S => A, result: A) = {
    assert(tStates(t) == Out)
    tStates(t) = Pending(msg, op, result)
  }

  /** Undo invocation of op by t. */
  def uninvoke[A](t: Int, msg: String, op: S => A, result: A) = {
    assert(tStates(t) == Pending(msg, op, result))
    tStates(t) = Out
  }

  /** Fire t's operation if it produces the expected value. 
    * Pre: t has a pending operation. 
    * @return Either t's previous state (if successful) or the result 
    * produced (otherwise).  */
  def fire(t: Int) : Either[ThreadState,Any] = {
    val pe = tStates(t).asInstanceOf[Pending[S,_]]
    val result = pe.op(seqObj)
    if(pe.result == result){
      // println(stateWrapper.get+" -"+t+"."+msg+"-> "+newStateW.get)
      tStates(t) = Ret(result); Left(pe)
    }
    else{ seqObj.undo; Right(result) } // Can't linearize here
  }
  //  def fire(t: Int) : Option[ThreadState] = {
  //   val pe = tStates(t).asInstanceOf[Pending[S,_]]
  //   val result = pe.op(seqObj)
  //   if(pe.result == result){
  //     // println(stateWrapper.get+" -"+t+"."+msg+"-> "+newStateW.get)
  //     tStates(t) = Ret(result); Some(pe)
  //   }
  //   else{ seqObj.undo; None } // Can't linearize here
  // }
  
  /** Try to return t's operation, firing it if it hasn't already been fired
    * and produces the expected value.  
    * @return Either t's previous state (if successful) or the result 
    * produced (otherwise). */
  def fireRet(t: Int) : Either[ThreadState,Any] = {
    val e = tStates(t)
    if(e.isInstanceOf[Ret]) { tStates(t) = Out; Left(e) }
    else{
      val pe = e.asInstanceOf[Pending[S,_]]
      val result = pe.op(seqObj)
      if(pe.result == result){
	// println(stateWrapper.get+" -"+t+"."+msg+"-> "+newStateW.get)
	tStates(t) = Out; Left(e)
      }
      else{ seqObj.undo; Right(result) }
    }
  }

  /** Does t have a pending operation? */
  def hasPending(t: Int) : Boolean = tStates(t).isInstanceOf[Pending[S,_]]

  /** Can t's operation return? */
  def canReturn(t: Int) : Boolean = tStates(t).isInstanceOf[Ret]

  /** Record t's operation as returned, returning previous state. */
  def doReturn(t: Int) : ThreadState = {
    val prev = tStates(t); tStates(t) = Out; prev
  }

  /** Undo the last event, resetting t's state to prev. */
  def undo(t: Int, prev: ThreadState) = {
    assert(tStates(t).isInstanceOf[Ret] || tStates(t) == Out)
    if(prev.isInstanceOf[Pending[S,_]]) seqObj.undo
    tStates(t) = prev
  }
}

// -------------------------------------------------------

object UndoConfig{


}
