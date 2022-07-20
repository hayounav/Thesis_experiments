package ox.cads.testing
import ox.cads.util.{ThreadUtil, Profiler}
import ox.cads.collection.BoundedBuffer
import scala.collection.mutable.Undoable

/** A tester for linearizabilty.  
  * 
  * This is a generic competition tester, that takes two solvers as 
  * parameters, one QueueLinTester, and one generic.
  *
  * @tparam A the type of data stored in the queue
  * @tparam US the type of undoable sequential specification objects.
  * @tparam C the type of concurrent object being tested.
  * @tparam L the type of log used.
  *
  * @param mkLog a function to create the log, given the event creation 
  * functions of the generic solver.
  * @param worker a function giving the behaviour of a worker.
  * @param p the number of workers.
  * @param iters the number of operations performed by each worker. 
  * @param concQueue the concurrent queue object.
  * @param queueSolver the queue linearizability solver.
  * @param genericSolver the generic solver.
  */
class QueueCompetitionTester[A, S, C, L <: QueueCompetitionLog[A,S,C]](
  mkLog: (GenericLog.MkInvokeType[S], GenericLog.MkReturnType) => L,
  worker: QueueCompetitionTester.WorkerType[A,S,C], 
  p: Int, iters: Int, concQueue: C,
  queueSolver: QueueLinSolver[A], genericSolver: GenericSolver[S, Event])
{

  /** Run the linearizability solvers. */
  def apply() : Int = {
    val log = mkLog(genericSolver.mkInvoke, genericSolver.mkReturn)
    // Run the workers
    ThreadUtil.runIndexedSystem(p, t => worker(t, log(t)))
    // Test for linearizability
    solve(log.getLog)
  }
   
  /** Test whether the history given by events is linearizable, returning 
    * one of the values defined in Solver. */
  def solve(events: Array[(QueueLinNode, Event)]) : Int = 
    new CompetitionSolver(queueSolver, genericSolver).solve(events)
}

// --------- Companion object ---------

object QueueCompetitionTester{
  /** The type of workers for this type of tester. */ 
  type WorkerType[A,S,C] = (Int, QueueCompetitionThreadLog[A,S,C]) => Unit

  /** Make a log object. */
  private def mkLog[A, S, C](tsLog: Boolean, p: Int, iters: Int, concQueue: C)(
    mkInvoke: GenericLog.MkInvokeType[S], mkReturn: GenericLog.MkReturnType)
    : QueueCompetitionLog[A,S,C]
  = if(tsLog)
      new TSQueueCompetitionLog[A,S,C](p, iters, concQueue, mkInvoke, mkReturn)
    else
      new SharedQueueCompetitionLog[A,S,C](
	p*iters, concQueue, mkInvoke, mkReturn)

  // --------- Factory methods.
  
  /** A QueueCompetitionTester using the Wing & Gong Tree Search Algorithm.
    * @tparam A the type of data stored in the queue.
    * @tparam US the type of the undoable sequential specification datatype.
    * @tparam C the type of concurrent datatypes.   
    * @param worker a function that produces a worker.
    * @param p the number of threads.
    * @param iters the number of iters performed by each worker.
    * @param concQueue the concurrent queue object. 
    * @param us the undoable sequential specification datatype.
    * @param tsLog should a timestamp-based log be used? */
  def WG[A, US <: Undoable, C](
    worker: WorkerType[A,US,C], p: Int, iters: Int, 
    concQueue: C, us: US, tsLog: Boolean = true)
  = new QueueCompetitionTester(
      mkLog[A,US,C](tsLog, p, iters, concQueue), worker, p, iters, concQueue, 
      new QueueLinSolver(p), new WGLinearizabilityTester[US](us, p))

  /** A QueueCompetitionTester using the Just-in-Time Linearization Tree
    * Search Algorithm. 
    * @tparam A the type of data stored in the queue.
    * @tparam US the type of the undoable sequential specification datatype.
    * @tparam C the type of concurrent datatypes.   
    * @param worker a function that produces a worker.
    * @param p the number of threads.
    * @param iters the number of iters performed by each worker.
    * @param concQueue the concurrent queue object. 
    * @param us the undoable sequential specification datatype.
    * @param tsLog should a timestamp-based log be used? */
  def JIT[A, US <: Undoable, C](
    worker: WorkerType[A,US,C], p: Int, iters: Int, 
    concQueue: C, us: US, tsLog: Boolean = true)
  = new QueueCompetitionTester(
      mkLog[A,US,C](tsLog, p, iters, concQueue), worker, p, iters, concQueue, 
      new QueueLinSolver(p), new JITLinUndoTester[US](us, p))
}
