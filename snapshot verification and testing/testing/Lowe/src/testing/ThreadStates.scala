package ox.cads.testing

/** Each object of this class records the states of each thread within a 
  * configuration */
class ThreadStates[S](val states: Array[ThreadStates.ThreadState]){
  // ----- Query operations -----

  /** Does t have an operation pending? */
  def hasPending(t: Int) : Boolean = 
    states(t).isInstanceOf[ThreadStates.Pending[S,_]]

  /** Number of pending operations. */
  def numPending = {
    var count = 0
    for(i <- 0 until length) if (hasPending(i)) count += 1
    count
  }

  /** Get pending operation. */
  def getOp[A](t: Int) : (String, S => (Any, S), Any) = {
    val ThreadStates.Pending(msg, op : (S => (Any, S)), result) = states(t)
    (msg, op, result)
  }

  /** Can thread t return result? */
  def allowsReturn(t: Int, result: Any) : Boolean = 
    states(t) == ThreadStates.Ret(result)

  /** Can thread t return*/
  def allowsReturn(t: Int) : Boolean = states(t).isInstanceOf[ThreadStates.Ret]

  /** The number of states. */
  val length = states.length

  // ----- Destructive updates -----

  /** Add that op is invoked by t (destructive). */
  def invoke[A](t: Int, msg: String, op: S => (A, S), result: A) = {
    assert(states(t) == ThreadStates.Out)
    states(t) = ThreadStates.Pending(msg, op, result)
  }

  /** Log that t returns result (destructive). */
  def logReturn1(t: Int, result: Any) = {
    assert(states(t) == ThreadStates.Ret(result))
    states(t) = ThreadStates.Out
  }

  /** Log that t fired op and returned (destructive). */
  def logFireReturn(t: Int) = {
    assert(states(t).isInstanceOf[ThreadStates.Pending[S,_]])
    states(t) = ThreadStates.Out
  }

  // ----- Non-destructive updates -----

  /** Create new ThreadStates corresponding to op being invoked by t.  
    * (Non-destructive.) */
  def logInvoke[A](t: Int, msg: String, op: S => (A, S), result: A) 
  : ThreadStates[S] = {
    assert(states(t) == ThreadStates.Out)
    val newStates = states.clone
    newStates(t) = ThreadStates.Pending(msg, op, result)
    new ThreadStates(newStates)
  }

  /** Give new ThreadStates object corresponding to t doing op 
    * producing res (non-destructive). */ 
  def logFire[A](t: Int, res: A) : ThreadStates[S] = {
    assert(states(t).isInstanceOf[ThreadStates.Pending[S,A]])
    val newStates = states.clone
    newStates(t) = ThreadStates.Ret(res)
    new ThreadStates(newStates)
  }

  /** Give new ThreadStates object corresponding to t having returned result
    * (non-destructive). */
  def logReturn(t: Int, result: Any) : ThreadStates[S] = {
    assert(states(t) == ThreadStates.Ret(result))
    logReturn(t)
  }  

  /** Give new ThreadStates object corresponding to t having returned
    * (non-destructive). */
  def logReturn(t: Int) : ThreadStates[S] = {
    val newStates = states.clone; newStates(t) = ThreadStates.Out
    new ThreadStates(newStates)
  }

  /** Log that t fired op and returned (non-destructive). */
  def logFireReturn1(t: Int)  : ThreadStates[S] = {
    assert(states(t).isInstanceOf[ThreadStates.Pending[S,_]])
    val newStates = states.clone; newStates(t) = ThreadStates.Out
    new ThreadStates(newStates)
  }

  // ----- Misc operations -----

  override def toString = 
    (0 until length) . map(i => i+": "+states(i)) . mkString("<", "; ", ">")

  override def equals(other: Any) = other match{ 
    case (t: ThreadStates[S]) => {
      var i = 0
      while(i < length && states(i) == t.states(i)) i += 1
      i==length
    }
  }

  override def hashCode = {
    var h = 0x3c074a61; var ix = 0
    while(ix < states.size){ 
      // h = h * 0xb592f7af + states(ix).hashCode; ix += 1 
      h = h & states(ix).hashCode; ix += 1 
    }
    h
  }
}

// -------------------------------------------------------

/** Types related to thread states. */
object ThreadStates{
  /** The type of states of threads. */
  abstract class ThreadState

  /** A thread has invoked an operation described by msg, corresponding
    * to operation op on the sequential object; this operation has not
    * yet been linearized. */
  case class Pending[S,A](msg: String, op: S => (A, S), result: A) 
  extends ThreadState{
    override def toString = msg
  }

  /** A thread has had an operation linearized, producing res, but this has
    * not yet returned. */
  case class Ret(res: Any) extends ThreadState

  /** A thread is not currently in any operation call */
  case object Out extends ThreadState

  /** Initial states for p threads. */
  def apply[S](p: Int) = new ThreadStates[S](Array.fill(p)(Out))
}
