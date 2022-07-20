package ox.cads.testing
import ox.cads.util.{ThreadUtil, Profiler}
import scala.reflect.ClassTag

/** Competition parallel competition of two solvers. */
class CompetitionSolver[E1 <: Event, E2 <: Event]
  (solver1: Solver[E1], solver2: Solver[E2])
  (implicit m1: ClassTag[E1], m2: ClassTag[E2])
{
  /** Run the two solvers on their components of events. */
  def solve(events: Array[(E1,E2)]) : Int = {    
    val results = Array.fill(2)(Solver.Error) // For storing results
    // Run the solvers
    def thread1 = {
      results(0) = solver1.solve(events.map(_._1).toArray);
      if(results(0) != Solver.OutOfSteam) solver2.interrupt 
    }
    def thread2 = {
      results(1) = solver2.solve(events.map(_._2).toArray)
      if(results(1) != Solver.OutOfSteam) solver1.interrupt
    }
    ThreadUtil.runParallel(thread1, thread2)
    // Extract the result
    if(results(0) == Solver.Error || results(1) == Solver.Error){
      // One of the threads threw an error
      println("Error! "+(results(0),results(1))); sys.exit
    }
    if(results(1) == Solver.Interrupted || results(1) == Solver.OutOfSteam){ 
      /*Profiler.count("solver1"); */ results(0) 
    }
    else if(results(0) == Solver.Interrupted || 
	    results(0) == Solver.OutOfSteam){
      /*Profiler.count("solver2"); */ results(1)
    }
    else{ 
      // Profiler.count("draw"); 
      if(results(0) != results(1)){ 
	println("results differ! "+(results(0),results(1))); sys.exit
      }
      results(0) 
    }
  }
}
  
