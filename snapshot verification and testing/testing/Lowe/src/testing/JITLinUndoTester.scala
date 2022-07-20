package ox.cads.testing

import ox.cads.util.Profiler
import scala.collection.mutable.ArrayBuffer

/** A linearizability tester based on just-in-time linearization, depth-first 
  * tree search, and an undoable sequential specification object. 
  *
  * @tparam S the type of states of the corresponding sequential datatype.
  * @tparam C the type of states of the concurrent datatype being tested.
  * 
  * @param seqObj the sequential specification datatype.
  * @param concObj the concurrent object.
  * @param p the number of workers.
  * @param worker a worker thread, whose parameters will be its identity and 
  * this.
  * @param invocs the maximum number of invocations that will be logged.
  * @param the maximum number of iterations.
  * @param verbose flag to indicate verbose messages.
  */

class JITLinUndoTester[S <: scala.collection.mutable.Undoable](
  seqObj: S, p: Int, maxSize: Long = -1, verbose: Boolean = false)
extends GenericSolver[S, Event]{
  // TODO: make type Event more specific
  // def mkInvoke(t: Int, msg: String, seqOp: S => Any) =  
  //   new LLInvokeEvent(t, msg, seqOp)

  // def mkReturn(t: Int, result: Any) = new LLReturnEvent(t, result)

  val mkInvoke = new LLInvokeEvent[S,Any,Any](_,_,_)
  val mkReturn = new LLReturnEvent[Any](_,_)

  /** The events performed by the threads. */
  private var events : Array[Event] = null

  /** The states of threads. */
  private val config = new UndoConfig(seqObj, p)
  private type ThreadState = config.ThreadState

  // The following three variables are used for producing debuging output.
  /** The maximum point in the history reached. */
  private var maxReached = 0
  /** For each thread, the maximum point reached in the history. */ 
  private val maxReachedFor = new Array[Int](p)
  /** For each thread t, the results that are allowed for the return in 
    * position maxReachedFor(t). */
  private val allowedResults = Array.fill(p)(new ArrayBuffer[Any]())

  // Two versions of the algorithm are presented.  The recursive version is
  // probably the easier to understand.  However, it runs out of stack space
  // on moderately sized histories.  Therefore the iterative version is the
  // default.

  /** Previous solve method, using recursion.  
    * This runs out of stack space on long histories (e.g. 512). */
  private def solveRec(es: Array[Event]) : Int = {
    events = es // copy into global variable
    val result = solve(0)
    if(result == Solver.Failure) debug(es)
    result      
  }  

  /** Print debug information. */
  private def debug(events: Array[Event])  = {
    println("Error found")
    assert(maxReached < events.size)
    for(i <- 0 to maxReached) println(events(i))
    val t = events(maxReached).asInstanceOf[ReturnEvent[Any]].t
    assert(allowedResults(t) != null)
    println("-- Previous event not linearized")
    println("-- Allowed return values: "+
	    allowedResults(t).distinct.mkString(", "))
  }

  /** Solve, starting from step i */
  private def solve(i: Int) : Int = {
    // Profiler.count("iter")
    if(interrupted) return Solver.Interrupted
    if(i > maxReached){ maxReached = i /*; allowedResults.clear */ }
    if(i == events.size) return Solver.Success //done!
    events(i) match{
      case im @ InvokeEvent(t, msg, op) => {
	if(verbose) println((seqObj,i)+": "+t+" invokes "+msg)
	if(i > maxReachedFor(t)){
	  maxReachedFor(t) = i; allowedResults(t).clear
	}
	val op1 = op.asInstanceOf[S => Any]
	config.invoke(t, msg, op1, im.ret.result)
	val res = solve(i+1) // move on to next event
	if(res != Solver.Failure) return res //done, or interrupted
	// else undo
	if(verbose) println("Undoing: "+t+" invokes "+msg)
	config.uninvoke(t, msg, op1, im.ret.result)
	Solver.Failure
      }
      case ReturnEvent(t, result) => {
	if(i > maxReachedFor(t)) maxReachedFor(t) = i
	if(verbose) println((seqObj,i)+": "+t+" returns "+": "+result)
	if(config.canReturn(t)){ 
	  val prev = config.doReturn(t)
	  val res = solve(i+1) // move on to next event
	  if(res != Solver.Failure) return res //done, or interrupted
	  else config.undo(t, prev)
	  Solver.Failure
	}
	// Get all perms of threads with pending ops
	else 
	  fireOthersThen(t, i)
      }
    } // end of match
  }

  /** Try firing some operations other than t, then fire t, and continue from
    * event i+1. */
  private def fireOthersThen[A](t: Int, i: Int) : Int = {
    // Consider firing t1, where t1 starts with t, and then iterates over
    // threads other than t; i.e. consider threads in order t, 0, 1, ..., t-1,
    // t+1, ... p-1.
    var t1 = t
    while(t1 < p){
      if(config.hasPending(t1) || t==t1){
	// Try firing t1's op, optionally obtaining t1's previous state
	val oPrev = if(t==t1) config.fireRet(t) else config.fire(t1)
	oPrev match{
	  case Left(a) => {
	    // op successfully fired; t1's previous state was a
	    if(verbose) println(i+": Fired "+t1+": "+seqObj)
	    val res = if(t1 == t) solve(i+1) else fireOthersThen(t, i)
	    if(res != Solver.Failure) return res //done or interrupted
	    // otherwise undo and try next option
	    if(verbose) println(i+": Undoing: "+t1+" firing")
	    config.undo(t1, a)
	  }
	  case Right(b) => {
	    // t1's op couldn't be fired in this state
	    if(verbose) println("Failed to fire "+t1)
	    if(i == maxReachedFor(t1)) allowedResults(t1) += b
	    //  try next option
	  }
	}
      } // end of if(config.hasPending(t1) || t==t1)
      // Move on to next thread to try.
      t1 = next(t, t1) 
      // if(t1 == t && t1 != 0) t1 = 0 // after t, consider 0 (if t != 0)
      // else if(t1+1 != t) t1 = t1+1  // next in line
      // else t1 = t1+2 
      // skip over t1
    } // end of while loop
    // All options failed
    Solver.Failure
  }

  /** Next thread to consider after t1, above.  
    * When t1 is started at t, this will iterate over the threads in the
    * order t, 0, 1, ..., t-1, t+1, ... p-1. */ 
  private def next(t: Int, t1: Int) : Int = 
    if(t1 == t && t1 != 0) 0 // after t, consider 0 (if t != 0)
    else if(t1+1 != t) t1+1  // next in line
    else t1+2 


  // --------- Iterative version of solve ---------

  /** Objects stored on the stack. */
  private abstract class StackObject

  /** Try firing the i'th event */
  private case class Solve(i: Int) extends StackObject

  /** Uninvoke operation op by t. */
  private case class Uninvoke(t: Int, msg: String, op: S => Any, result: Any)
	       extends StackObject

  /** Try firing some operations other than t, starting from t1; then fire t;
    * and continue from event i+1. */
  private case class FireOthersThen(t: Int, i: Int, t1: Int) 
	       extends StackObject

  /** Create FireOthersThen object corresponding to firing next thread 
    * after t1. */
  private def nextFireEvent(t: Int, i: Int, t1: Int) : FireOthersThen = {
    val t2 = next(t, t1)
    if(t2 < p) FireOthersThen(t, i, t2) else null
  }

  /** Undo t1's op to previous state a; then continue as in 
    * FireOthersThen(t, i, next(t1)). */
  private case class UndoFireOthers(t: Int, i: Int, t1: Int, a: ThreadState) 
	       extends StackObject

  /** Solve, iteratively. */
  def solve(es: Array[Event]) : Int = {
    val stack = new scala.collection.mutable.Stack[StackObject]()
    var current : StackObject = Solve(0)
    var count = 0L // steps so far

    // At each point, we have to perform the action corresponding to current,
    // if non-null, then continue with the actions corresponding to the stack.
    while(current != null || stack.nonEmpty){
      if(interrupted) return Solver.Interrupted
      count += 1
      // if(count%100000000 == 0) print(count)
      if(count == maxSize){ 
	println("JIT Tree Search giving up"); return Solver.OutOfSteam
      }
      if(current == null) current = stack.pop
      current match{
	case Solve(i) => {
	  if(i == es.size) return Solver.Success //done!
	  if(i > maxReached){ maxReached = i /*; allowedResults.clear */ }
	  es(i) match{
	    case im @ InvokeEvent(t, msg, op) => {
	      if(verbose) println((seqObj,i)+": "+t+" invokes "+msg)
	      if(i > maxReachedFor(t)){
		maxReachedFor(t) = i; allowedResults(t).clear
	      }
	      val op1 = op.asInstanceOf[S => Any]
	      config.invoke(t, msg, op1, im.ret.result)
	      current = Solve(i+1) // continue from next event
	      // If unsuccessful, undo that operation
	      stack.push(Uninvoke(t, msg, op1, im.ret.result))
	    }
	    case ReturnEvent(t, result) => {
	      if(i > maxReachedFor(t)) maxReachedFor(t) = i
	      if(verbose) println((seqObj,i)+": "+t+" returns "+": "+result)
 	      current = FireOthersThen(t, i, t)
	    }
	  } // end of es(i) match
	} // end of case Solve(i)
	case Uninvoke(t, msg, op, result) => {
	  if(verbose) println("Undoing: "+t+" invokes "+msg)
	  config.uninvoke(t, msg, op, result)
	  current = null // continue from stack
	} // end of case Uninvoke(t, msg, op, result)
	case FireOthersThen(t, i, t1) => { 
	  if(config.hasPending(t1) || t==t1){
	    // Try firing t1's op, optionally obtaining t1's previous state
	    val oPrev = if(t==t1) config.fireRet(t) else config.fire(t1)
	    oPrev match{
	      case Left(a) => {
		// op successfully fired; t1's previous state was a
		if(verbose) println(i+": Fired "+t1+": "+seqObj)
		// If we've fired t, continue from event i+1; otherwise try
		// firing some other threads. 
		current = if(t1 == t) Solve(i+1) else FireOthersThen(t, i, t)
		// If unsuccessful, undo that operation, and try firing the 
		// next thread.
		stack.push(UndoFireOthers(t, i, t1, a))
	      }
	      case Right(b) => { 
		// t1's op couldn't be fired in this state
		if(verbose) println("Failed to fire "+t1)
		if(i == maxReachedFor(t1) && !allowedResults(t1).contains(b)){
		  allowedResults(t1) += b
		  // Following is to limit memory usage.  Perhaps using a Set
		  // would be better.
		  // if(allowedResults(t1).size > 10000)
		  //   allowedResults(t1) = allowedResults(t1).distinct
		}
		current = 
		  if(t == t1 && config.canReturn(t)) 
		    // The JIT linearization says we shouldn't try linearizing
		    // other ops here.
		    null
		  else nextFireEvent(t, i, t1) // Try firing next thread
	      }
	    } // end of oPrev match
	  } // end of if(config.hasPending(t1) || t==t1)
	  else current = nextFireEvent(t, i, t1) // Try firing next thread
	} // end of case FireOthersThen(t, i, t1)
	case UndoFireOthers(t, i, t1, a) => {
	  if(verbose) println(i+": Undoing: "+t1+" firing")
	  config.undo(t1, a)
	  current = nextFireEvent(t, i, t1) // Try firing next thread
	} // end of case UndoFireOthers(t, i, t1, a)
      } // end of match
    } // end of while loop    
    
    // Search failed; give debugging info
    debug(es); Solver.Failure
  }





}
