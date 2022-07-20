package ox.cads.testing

import ox.cads.util.Profiler
import scala.collection.mutable.{HashSet,Stack}
import scala.language.existentials 

/** A tester for linearizabilty.
  *
  * This is the tester referred to as the ''Wing & Gong Graph-Search''
  * in the paper 
  * [[http://www.cs.ox.ac.uk/people/gavin.lowe/LinearizabiltyTesting/ ''Testing for Linearizability'', Gavin Lowe]].  
  * See that paper for more explanation of the technique. 
  *
  * The algorithm is based on  the algorithm in ''Testing and 
  * Verifying Concurrent Objects'', by Jeannette Wing and Chun Gong, extended 
  * to recognise revisited configurations.
  *
  * @tparam S the type of states of the corresponding sequential datatype 
  * @tparam C the type of states of the concurrent datatype being tested
  * @param seqObj the sequential object that acts as the specification for the
  * concurrent datatype
  * @param p the number of workers
  * @param maxSize the maximum number of configurations to allow
  * @param verbose should verbose output be given
  */
class WGGraph[S]
  (seqObj: S, p: Int, maxSize: Long = -1, verbose: Boolean = false)
extends GenericSolver[S, Event]{
  
  // def mkInvoke(t: Int, msg: String, seqOp: S => Any) =  
  //   new LLInvokeEvent(t, msg, seqOp)

  // def mkReturn(t: Int, result: Any) = new LLReturnEvent(t, result)
  val mkInvoke = new LLInvokeEvent[S,Any,Any](_,_,_)
  val mkReturn = new LLReturnEvent[Any](_,_)

  /** Run the linearization tester on the history given by events.
    * @return Solver.Success to indicate the linearization succeeded; 
    * Solver.OutOfSteam to indicate that the test gave up; or 
    * Solver.Failure to indicate that the linearization failed. */
  def solve(events: Array[Event]) : Int = { 
    val invocs = events.length

    // Build linked list
    val logHeader = 
      LLNode.buildList(events.asInstanceOf[Array[LLNode.LLEvent]])
    if(verbose) LLNode.printLog(logHeader)

    // LLNode.printLog(logHeader)

    // We now try to linearize the history represented by the log.  Each
    // linearization is formed by picking the first operation to linearize:
    // the invocation event must be before the first return event, in order to
    // respect the happens-before relation.  We then seek to linearize the
    // remaining events.  The following variable represents the next event to
    // consider.
    var nextEv = logHeader.next // next event to try 

    // We use a stack (a) to record which operations have been linearized so
    // far; and (b) to allow back-tracking.  In particular, each prefix
    // es^<(s,e,pl)> of the stack indicates that we still have to consider the
    // possible linearizations formed by re-inserting map snd es^<e> into the
    // log, and then trying to consider linearizing events after e, starting
    // from state s.  The third component shows which prefix of return events
    // are linearized.
    val stack = 
      new Stack[(StateWrapper[S], LLInvokeEvent[S,_,_], Int)]()
    // We use a wrapper around the sequential object
    var wrapper = new StateWrapper(seqObj)
    // Events currently linearized
    val linearized = new MyBitMapSet(invocs)
    var count = 0L // # events considered so far. 
    // The first prefixLinearized events have been linearized 
    // (but not the next).
    var prefixLinearized = 0 
    var maxPrefixLinearized = 0 // max value of prefixLinearized
    // We record which configurations (states of seqObj, linearized
    // operations) we have seen so far.
    var seen = new java.util.HashSet[(StateWrapper[S], MyBitMapSet)](8*invocs)

    while(logHeader.next != null){
      // Profiler.count("iteration")
      count += 1
      if(interrupted) return Solver.Interrupted 
      if(count == maxSize){ 
	println("WG Graph Search giving up")
	// Try to reclaim memory
	// seen = null; java.lang.System.gc; println("gc done")
	return Solver.OutOfSteam 
      }
      nextEv match {
	case ie : LLInvokeEvent[S,_,(_,S)] @unchecked => {
	  val ret = ie.ret.asInstanceOf[LLReturnEvent[Any]] // return event
	  // See if the operation can be linearized here
	  val (seqResult, newWrapper) = wrapper.doOp(ie.msg, ie.op)
	  if(ret.compareResult(seqResult)){
	    // Can be linearised; create next configuration
	    val newConfig = (newWrapper, linearized+ret.index)
	    if(seen.add(newConfig)){
	      // This is a new configuration
	      // store backtrack info
	      stack.push((wrapper, ie, prefixLinearized))
	      linearized += ret.index // we've linearized ie/ret
	      LLNode.lift(ie, ret) // remove from history
	      // update prefixLinearized/maxPrefixLinearized
	      while(prefixLinearized < invocs && linearized(prefixLinearized))
	        prefixLinearized += 1 
	      maxPrefixLinearized = maxPrefixLinearized max prefixLinearized
	      wrapper = newWrapper // continue from new state
	      nextEv = logHeader.next  // Reset to find next op to linearize
	    } // end of if(seen.add(newConfig))
	    else // rest of history can't be linearised, so advance to next op
	      nextEv = nextEv.next
	  } // end of if(ret.compareResult(seqResult))
	  else  nextEv = nextEv.next // advance to next op
	} // end of case ie @ InvokeEvent(t, msg, op)
	case re : LLReturnEvent[_] => {
	  // Try backtracking
	  if(stack.isEmpty){ 
	    println("Impossible return: "+re.result); 
	    debug(maxPrefixLinearized, logHeader)
	    return Solver.Failure
	  } 
	  // old state; event to undo; old value of prefixLinearized
	  val (newWrapper, undoEv, pl) = stack.pop 
	  wrapper = newWrapper; LLNode.unlift(undoEv) // re-insert in log
	  linearized -= undoEv.ret.asInstanceOf[LLReturnEvent[_]].index 
	  // no longer linearized
	  prefixLinearized = pl
	  nextEv = undoEv.next  // Advance to next event
	} // end of case ReturnEvent(t, result)
      }
    }

    Solver.Success
  } // end of apply

  /** Produce debugging information then exit */
  private def debug(maxPrefixLinearized: Int, logHeader: Event with LLNode) = {
    // The operations with return indices [0..maxPrefixLinearised) can be
    // linearised, but not also the following event with index
    // maxPrefixLinearised.
    var e = logHeader.next; var done = false
    val fw = new java.io.FileWriter("DFSerror.txt")
    def write(st: String) = { println(st); fw.write(st+"\n") }

    // write events until first that hasn't been linearized
    try{
      while(!done){
	write(e.toString)
	e match{
	  case re: LLReturnEvent[_] => done = re.index == maxPrefixLinearized
	  case ie: LLInvokeEvent[S,_,_] => { 
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
      write("\t-- Allowed return values: " + re.results.map(show).mkString(", "))
    }
    finally{ fw.close }
  }

}
