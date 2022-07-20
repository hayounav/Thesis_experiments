package ox.cads.testing

import ox.cads.util.Profiler
import scala.language.existentials 

/** A linearization tester.
  * 
  * This is the tester referred to as the ''Wing & Gong Tree Search 
  * Algorithm'' in the paper 
  * [[http://www.cs.ox.ac.uk/people/gavin.lowe/LinearizabiltyTesting/ ''Testing for Linearizability'', Gavin Lowe]].  
  * See that paper for more explanation of the technique. 
  *
  * The algorithm is based on ''Testing and Verifying
  * Concurrent Objects'', by Jeannette Wing and Chun Gong.
  *
  * @tparam S the type of sequential spedicifation objects
  * @tparam C the type of the concurrent objects
  * @param seqObj the sequential specification object
  * @param concObj the concurrent object
  * @param p the number of workers
  * @param worker a function giving the behaviour of a worker; the first 
  *  parameter of worker represents the worker's identity, and the second 
  *  the linearization tester.
  * @param invocs the number of event invocations
  * @param maxSize the maximum number of configurations to allow
  * @param verbose should verbose output be given. */
class WGLinearizabilityTester[S <: scala.collection.mutable.Undoable](
  seqObj: S, p: Int, maxSize: Long = -1, verbose: Boolean = false)
extends GenericSolver[S, Event]{

  // protected[testing] def mkInvoke(t: Int, msg: String, seqOp: S => Any) = 
  //   new LLInvokeEvent(t, msg, seqOp)

  // protected[testing] def mkReturn(t: Int, result: Any) = 
  //   new LLReturnEvent(t, result)
  val mkInvoke = new LLInvokeEvent[S,Any,Any](_,_,_)
  val mkReturn = new LLReturnEvent[Any](_,_)

  /** Run the linearization tester, assuming the log has already been 
    * generated. 
    * @return Solver.Success to indicate the linearization succeeded; 
    * Solver.OutOfSteam to indicate that the test gave up; or 
    * Solver.Failure to indicate that the linearization failed. */
  def solve(events : Array[Event]) : Int = {    
    // Build linked list using the events received from the threads.
    val logHeader = 
      LLNode.buildList(events.asInstanceOf[Array[LLNode.LLEvent]])
    if(verbose) LLNode.printLog(logHeader)

    // We now try to linearize the history represented by the log.  Each
    // linearization is formed by picking the first operation to linearize:
    // the invocation event must be before the first return event, in order to
    // respect the happens-before relation.  We then seek to linearize the
    // remaining events.  The following variable represents the next event to
    // consider.
    var nextEv = logHeader.next // next event to try 
    // We use a stack (a) to record which operations have been linearized so
    // far; and (b) to allow back-tracking.  In particular, each prefix es^<e>
    // of the stack indicates that we still have to consider the possible
    // linearizations formed by re-inserting es^<e> into the log, and then
    // trying to consider linearizing events after e.  The third component 
    // shows which prefix of return events are linearized.
    val stack = 
      new scala.collection.mutable.Stack[(LLInvokeEvent[S,_,_], Int)]()
    var count = 0L // # events considered so far. 
    val invocs = events.length / 2 // # operation invocations
    // Events currently linearized
    val linearized = new MyBitMapSet(invocs)
    // The first prefixLinearized events have been linearized 
    // (but not the next).
    var prefixLinearized = 0 
    var maxPrefixLinearized = 0 // max value of prefixLinearized
    
    while(logHeader.next != null){
      // Profiler.count("iter")
      count += 1
      if(count == maxSize){ 
	println("WG Tree Search giving up"); return Solver.OutOfSteam
      }
      if(interrupted) return Solver.Interrupted 
      nextEv match {
	case ie : LLInvokeEvent[S,_,_] => {
	  val ret = ie.ret.asInstanceOf[LLReturnEvent[Any]] // return event
	  // See if the operation can be linearized here
	  val seqResult = ie.op(seqObj) // result of sequential op
	  if(ret.compareResult(seqResult)){ 
	    // store backtrack info; remove from history
	    stack.push((ie, prefixLinearized)); LLNode.lift(ie, ret) 
	    linearized += ret.index // we've linearized ie/ret
	    // update prefixLinearized/maxPrefixLinearized
	    while(prefixLinearized < invocs && linearized(prefixLinearized))
	      prefixLinearized += 1 
	    maxPrefixLinearized = maxPrefixLinearized max prefixLinearized
	    // Reset to find next op to linearize
	    nextEv = logHeader.next
	  }
	  else{ seqObj.undo; nextEv = nextEv.next }
	}
	case e : LLReturnEvent[_] => {
	  // Try backtracking
	  if(stack.isEmpty){ 
	    println("Impossible return: "+e.result) 
	    debug(maxPrefixLinearized, logHeader) 
	    return Solver.Failure
	  }
	  val (undoEv, pl) = stack.pop // events to undo 
	  // Undo undoEv; re-insert in log
	  seqObj.undo; LLNode.unlift(undoEv)
	  prefixLinearized = pl
	  nextEv = undoEv.next  // Advance to next event
	}
      }
    }
    return Solver.Success
  }

  /** Produce debugging information then exit */
  private def debug(maxPrefixLinearized: Int, logHeader: Event with LLNode) = {
    var e = logHeader.next; var done = false
    val fw = new java.io.FileWriter("WGerror.txt")
    def write(st: String) = { println(st); fw.write(st+"\n") }

    // write events until first that hasn't been linearized
    try{
      while(!done){
	write(e.toString)
	e match{
	  case re: LLReturnEvent[_] => done = re.index == maxPrefixLinearized
	  case ie: LLInvokeEvent[_,_,_] => { 
	    val ret = ie.ret.asInstanceOf[LLReturnEvent[_]]
	    if(ret.index == maxPrefixLinearized)
	      write("\t-- Previous event not linearized")
	  }
	} // end of match
	if(!done) e = e.next
      }
      write("\t-- Previous event not linearized")
      val re = e.asInstanceOf[LLReturnEvent[Any]]
      def show(x: Any) = if(x==null) "null" else x.toString
      write("\t-- Allowed return values: " + 
	    re.results.map(show).mkString(", "))
    }
    finally{ fw.close }
  }
}
