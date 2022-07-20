package ox.cads.testing

/** An abstract class for linearizability algorithms. 
  * @tparam S the type of sequential specification objects. */

abstract class Solver[E <: Event]{
  /** Run the linearization tester on events. 
    * @return one of the values defined in the Solver companion object. */
  def solve(events : Array[E]) : Int 

  /** Has this tester been interrupted?
    *  
    * Note that the solve method must detect that this variable has been set,
    * and return Solver.Interrupted. */
  @volatile protected[testing] var interrupted = false

  /** Interrupt this thread.  */
  def interrupt = interrupted = true
}

// --------- Companion Object ---------

object Solver{
  /** A result returned by a linearizability solver to indicate the history 
    * was linearizable. */
  val Success = 1

  /** A result returned by a linearizability solver to indicate that the solver
    * reached its limit. */
  val OutOfSteam = 0

  /** A result returned by a linearizability solver to indicate that the solver
    * was interruptd. */
  val Interrupted = -1

  /** A result returned by a linearizability solver to indicate the history 
    * was not linearizable. */
  val Failure = -2

  /** A result returned by a linearizability solver to indicate that 
    * something went wrong.  */
  val Error = -3

}

