package ox.cads.testing
import ox.cads.util.Profiler
import scala.collection.mutable.ArrayBuffer

/** A class performing linearizability testing on concurrent queues.
  * @tparam A the type of data stored in the queue.
  * @param p the number of workers.
  * @param verbose flag to show if verbose output is required. 
  */
class QueueLinSolver[A](p: Int, verbose: Boolean = false)
extends Solver[QueueLinNode]{

  /** Test the history given by events for linearizability. */
  def solve(events: Array[QueueLinNode]) : Int = /* Profiler.time("solve")*/ {
    // The object representing the history
    var history = new QLTHistory[A](events, p)
    //Info for backtracking; what is the maximum initial sequence number for a
    //dequeue return that we've reached, and what were valid returns for that?
    var maxDeqReached = -1; var validEnqs : ArrayBuffer[Option[A]] = null

    // Stack for storing backtracking information when multiple
    // (non-earliest-ending) minimal matching pairs are found; the first two
    // components are the thread ids of the enqueueing and dequeueing threads;
    // the third is a clone of the state of the list.
    type StackInfo = (Int, Int, Seq[QueueLinNode])
    val stack = new scala.collection.mutable.Stack[StackInfo]

    while(true){
      if(interrupted) return Solver.Interrupted
      // if(verbose){ println; println(listToString); println }
      if(history.noDequeues){ // history contains only enqueues
	return Solver.Success // done
      }
      val deq = history.minUnsuccDeq // a minimal unsuccessful dequeue (or null)
      if(deq != null){
	if(verbose) println("Removing unsuccessful dequeue by "+deq.t)
	history.removeDequeue(deq)
      }
      else history.earliestEndingMinMatchingPair match{
	case Some((e,d)) => history.removeMatchingPair(e, d)
	case None => {
	  val allMatches = history.allMinMatchingPairs
	  if(allMatches.nonEmpty){
	    if(allMatches.length > 1){ // store backtrack info
	      // Profiler.count("Storing backtrack")
	      for((e1,d1) <- allMatches.tail)
		stack.push((e1.t, d1.t, history.cloneList))
	    }
	    // Try linearizing first pair
	    val (e,d) = allMatches.head; history.removeMatchingPair(e, d)
	  } // end of if(allMatches.nonEmpty)
	  else{ // need to try backtracking
	    // Store debug information
	    val (deqIx, enqVals) = history.getDebugInfo
	    if(deqIx > maxDeqReached){ 
	      maxDeqReached = deqIx; validEnqs = enqVals 
	    }
	    else if(deqIx == maxDeqReached) validEnqs ++= enqVals
	    if(stack.nonEmpty){    // pop from stack and backtrack
	      // Profiler.count("Backtracked")
	      val (et, dt, oldList) = stack.pop
	      history = new QLTHistory[A](oldList.toArray, p)
	      // find matching events
	      history.removeMatchingPairFor(et, dt)
	      // println("backtracking to "+e+"; "+d) // and continue
	    }
	    else{
	      println("non-linearizable")
	      // Print debug info; events(i) has index i+1 (confusingly)
	      for(i <- 0 until maxDeqReached) println(events(i))
	      println("\t-- Previous event not linearized")
	      println("\t-- Allowed return values: "+
		      validEnqs.distinct.mkString(", ")+"\n")
	      return Solver.Failure
	    }
	  } // end of case None
	} // end of history.earliestEndingMinMatchingPair match
      }
    } // end of while(true)

    sys.error("unreachable")
  }
}
