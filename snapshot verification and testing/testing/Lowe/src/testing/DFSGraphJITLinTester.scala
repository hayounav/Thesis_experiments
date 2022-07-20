package ox.cads.testing
import ox.cads.util.Profiler
import scala.collection.mutable.ArrayBuffer

/** A tester for linearizabilty.  
  * 
  * This tester uses a depth-first graph search, combined with just-in-time 
  * linearizability. 
  *
  * @tparam S the type of states of the corresponding sequential datatype 
  * @tparam C the type of states of the concurrent datatype being tested
  * @param seqObj the sequential object that acts as the specification for the
  * concurrent datatype
  * @param concObj the concurrent object being tested
  * @param p the number of workers
  * @param worker a worker thread, whose parameters will be its identity and 
  * this
  * @param invocs the maximum number of invocations that will be logged
  * @param maxSize the maximum number of configurations to allow
  * @param verbose should verbose output be given  */

class DFSGraphJITLinTester[S](
  seqObj: S, p: Int, maxSize: Long = -1, verbose: Boolean = false)
extends GenericSolver[S, Event]{
  val mkInvoke = new InvokeEvent[S,Any,Any](_,_,_)
  val mkReturn = new ReturnEvent[Any](_,_)

  private type Config = Configuration[S]

  /** A node of the search space. */
  private abstract class SearchNode

  /** Try firing the i'th event within config. */
  private case class Solve(config: Config, i: Int) extends SearchNode

  /** Starting from config, try firing some operations other than t, 
    * starting from t1; then fire t; and continue from event i+1. */
  private case class FireOthersThen(config: Config, t: Int, i: Int, t1: Int) 
	       extends SearchNode

  /** Create FireOthersThen object corresponding to firing next thread 
    * after t1. 
    * When t1 is started at t, this will iterate over the threads in the
    * order t, 0, 1, ..., t-1, t+1, ... p-1. */
  private def nextFireEvent(
    config: Config, t: Int, i: Int, t1: Int) : FireOthersThen
  = {
    // Next thread to fire
    val t2 = 
      if(t1 == t && t1 != 0) 0 // after t, consider 0 (if t != 0)
      else if(t1+1 != t) t1+1  // next in line (includes case t1 == t == 0)
      else t1+2                // skip over t
    if(t2 < p) FireOthersThen(config, t, i, t2) else null
  }

  // The following three variables are used for producing debuging output.
  /** The maximum point in the history reached. */
  private var maxReached = 0
  /** For each thread, the maximum point reached in the history. */ 
  private val maxReachedFor = new Array[Int](p)
  /** For each thread t, the results that are allowed for the return in 
    * position maxReachedFor(t). */
  private val allowedResults = Array.fill(p)(new ArrayBuffer[Any]())

  /** Test whether the history given by events is linearizable, returning 
    * one of the values defined in Solver. */
  def solve(events: Array[Event]) : Int = {
    // Initial configuration.
    val initConf = new Config(seqObj, ThreadStates[S](p))
    // Current node in the search space to work on
    var current : SearchNode = Solve(initConf,0)
    // Stack used to control backtracking.  (In fact, we only ever store
    // FireOthersThen objects on the stack.)
    val stack = new scala.collection.mutable.Stack[SearchNode]()
    // Set of SearchNodes seen previously.  (In fact we only store Solve
    // objects in the seen set.)
    val seen = new java.util.HashSet[SearchNode] // (4*invocs)
    // val seen = new scala.collection.mutable.HashSet[ExtConf]
    seen.add(current)
    var count = 0 // # iterations

    while(current != null || stack.nonEmpty){
      if(interrupted) return Solver.Interrupted
      count += 1
      if(count == maxSize){
	println("JIT Graph Search giving up"); return Solver.OutOfSteam
      }
      //Profiler.count("iter")
      if(current == null) current = stack.pop 
      current match{
	case Solve(config,i) => {
	  if(i == events.size) return Solver.Success //done!
	  if(i > maxReached) maxReached = i
	  events(i) match{
       	    case im @ InvokeEvent(t, msg, op: (S => (Any,S)) @unchecked) => {
	      // log += (t+" invokes "+msg)
	      if(verbose) println((config,i)+": "+t+" invokes "+msg)
	      if(i > maxReachedFor(t)){
		maxReachedFor(t) = i; allowedResults(t).clear
	      }
	      val newConfig = config.logInvoke(t, msg, op, im.ret.result)
	      if(verbose) println("Adding "+newConfig+", "+(i+1))
	      current = Solve(newConfig, i+1)
	      if(!seen.add(current)) current = null // avoid repeating 
	    } // end of case InvokeEvent
	    case ReturnEvent(t, result) => {
	      if(i > maxReachedFor(t)) maxReachedFor(t) = i
	      // log += (t+" returns "+msg+": "+result)
	      if(verbose) println((config,i)+": "+t+" returns: "+result)
	      current = FireOthersThen(config, t, i, t)
	    } // end of case ReturnEvent
	  } // end of events(i) match
	} // end of case Solve
	case FireOthersThen(config, t, i, t1) => { 
	  val next = nextFireEvent(config, t, i, t1) // next fire event
	  if(config.hasPending(t1) || t==t1){
	    // Try firing t1's op
	    val oPrev = if(t==t1) config.fireRet(t) else config.fire(t1)
	    oPrev match{
	      case Left(newConfig) => {
		// op successfully fired, giving newConfig
		if(verbose) println(i+": Fired "+t1+": "+seqObj)
		// Store the next possibility to come back to
		if(next != null) stack.push(next)
		// If we've fired t, continue from event i+1; otherwise try
		// firing some other threads. 
		if(t1 == t){
		  current = Solve(newConfig, i+1) 
		  if(!seen.add(current)) current = null // avoid repetition
		}
		else current = FireOthersThen(newConfig, t, i, t)
	      }
	      case Right(b) => { 
		// t1's op couldn't be fired in this state
		if(verbose) println("Failed to fire "+t1)
		// if(i == maxReached) allAllowedResults += b
		if(i == maxReachedFor(t1) && !allowedResults(t1).contains(b))
		  allowedResults(t1) += b
		// ((b, t, i, t1, config, stack.mkString("<","\n\t",">")))
		current = 
		  if(t == t1 && config.canReturn(t)) 
		    // The JIT linearization says we shouldn't try linearizing
		    // other ops here.
		    null
		  else next // Try firing next thread
	      }
	    } // end of oPrev match
	  } // end of if(config.hasPending(t1) || t==t1)
	  else current = next // Try firing next thread
	} // end of case FireOthersThen
      } // end of current match
      count += 1
      if(count == maxSize){ 
	println("JIT Graph Search giving up"); return Solver.OutOfSteam 
      }
    } // end of while loop

    // Search failed; give debugging info
    println("Error found") 
    val fw = new java.io.FileWriter("JITGerror.txt")
    def write(st: String) = { println(st); fw.write(st+"\n") }
    // Print each event in turn
    try{
      for(i <- 0 until events.length){
	write(i+": "+events(i))
	if(i == maxReached){
	  val t = events(maxReached).asInstanceOf[ReturnEvent[Any]].t
	  write("-- Previous event not linearized")
	  write("-- Allowed return values: "+
		allowedResults(t).distinct.mkString(", "))
	}
      }
    }finally{ fw.close }
    // for(i <- 0 to maxReached) println(i+": "+events(i))
    // val t = events(maxReached).asInstanceOf[ReturnEvent[Any]].t
    // println("-- Previous event not linearized")
    // println("-- Allowed return values: "+
    // 	    allowedResults(t).distinct.mkString(", "))
    // println("Out-standing returns:")
    // // Iterate through remaining events; for each thread t, if the first event
    // // of t we see is a return, then print it out.
    // val seenThread = new Array[Boolean](p)
    // for(i <- maxReached until events.length) events(i) match{
    //   case inv: InvokeEvent[S,Any,Any] => seenThread(inv.t) = true
    //   case ret: ReturnEvent[Any] =>
    // 	if(!seenThread(ret.t)){ println(i+": "+ret); seenThread(ret.t) = true }
    // }
    Solver.Failure  
  }
}
