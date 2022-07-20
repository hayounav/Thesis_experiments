package ox.cads.testing
import ox.cads.util.{ThreadUtil, Profiler}
import ox.cads.collection.BoundedBuffer
import scala.collection.mutable.Undoable

/** A tester for linearizabilty.  
  * 
  * This is a generic competition tester, that takes two solvers as 
  * parameters.
  *
  * @tparam S1 the first type of sequential specification objects.
  * @tparam S2 the second type of sequential specification objects.
  * @tparam C the type of concurrent object being tested.
  * @tparam L the typo of logs.
  *
  * @param mkLog function to create the log, given the event creation 
  * functions of the solvers.
  * @param solver1 the first solver.
  * @param solver2 the second solver.
  * @param worker a function giving the behaviour of a worker.
  * @param p the number of workers.
  * @param iters the number of event invocations by each worker.
  * @param concObj the concurrent object.
  */
class CompetitionTester[S1, S2, E1, E2, C, L <: CompetitionLog[S1, S2, C]](  
  mkLog: (GenericLog.MkInvokeType[S1], GenericLog.MkReturnType, 
	  GenericLog.MkInvokeType[S2], GenericLog.MkReturnType) => L,
  solver1: GenericSolver[S1,Event], solver2: GenericSolver[S2,Event],
  worker: CompetitionTester.WorkerType[S1, S2, C], 
  p: Int, iters: Int, concObj: C)
{
  /** Run the linearizability solvers. */
  def apply() : Int = {
    // Create the log
    val log = mkLog(solver1.mkInvoke, solver1.mkReturn,
		    solver2.mkInvoke, solver2.mkReturn)
    // Run the workers
    ThreadUtil.runIndexedSystem(p, t => worker(t, log(t)))
    // Test for linearizability 
    solve(log.getLog)
  }
   
  /** Test whether the history given by events is linearizable, returning 
    * one of the values defined in Solver. */
  def solve(events: Array[(Event,Event)]) : Int = 
    new CompetitionSolver(solver1, solver2).solve(events)
}

// --------- Companion object ---------

object CompetitionTester{
  /** The type of workers.  
    * The first parameter represents the worker's identity, and the second 
    * the log to use. */
  type WorkerType[S1, S2, C] = (Int, CompetitionThreadLog[S1,S2,C]) => Unit

  /** Make a log. */
  private def mkLog[S1, S2, C](tsLog: Boolean, p: Int, iters: Int, concObj: C)(
    mkInvoke1: GenericLog.MkInvokeType[S1], mkReturn1: GenericLog.MkReturnType,
    mkInvoke2: GenericLog.MkInvokeType[S2], mkReturn2: GenericLog.MkReturnType)
    : CompetitionLog[S1,S2,C]
  = if(tsLog)
      new TSCompetitionLog[S1,S2,C](
	p, iters, concObj, mkInvoke1, mkReturn1, mkInvoke2, mkReturn2)
    else 
      new SharedCompetitionLog[S1,S2,C](
	p*iters, concObj, mkInvoke1, mkReturn1, mkInvoke2, mkReturn2)

  /** Wing & Gong Graph-Search Algorithm. */
  private def WGG[S](s: S, p: Int, maxSize: Long = -1) = 
    new WGGraph[S](s, p, maxSize)
  /** Wing & Gong Tree-Search Algorithm */
  private def WGT[US <: Undoable](us: US, p: Int, maxSize: Long = -1) = 
    new WGLinearizabilityTester[US](us, p, maxSize)
  /** Just-in-time linearization tree-search algorithm. */
  private def JITT[US <: Undoable](us: US, p: Int, maxSize: Long = -1) =
    new JITLinUndoTester[US](us, p, maxSize)
  /** Just-in-time linearization graph-search algorithm. */
  private def JITG[S](s: S, p: Int, maxSize: Long = -1) = 
    new DFSGraphJITLinTester[S](s, p, maxSize)
  
  // --------- Factory methods ---------

  /** Produce a competition linearizability tester based on the Wing & Gong
    * Graph and Tree Search algorithms. 
    * @tparam S the type of the immutable sequential specification datatype.
    * @tparam US the type of the undoable sequential specification datatype.
    * @tparam C the type of concurrent datatypes.   
    * @param worker a function that produces a worker.
    * @param p the number of threads.
    * @param iters the number of iters performed by each worker.
    * @param concObj the concurrent object. 
    * @param s the immutable sequential specification datatype.
    * @param us the undoable sequential specification datatype.
    * @param tsLog should a timestamp-based log be used? */
  def WGWG[S, US <: Undoable, C](
    worker: WorkerType[S,US,C], p: Int, iters: Int, concObj: C, s: S, us: US, 
    tsLog: Boolean = true, maxSizeT: Long = -1, maxSizeG: Long = -1) 
  = new CompetitionTester(
      mkLog[S,US,C](tsLog, p, iters, concObj), 
      WGG(s,p,maxSizeG), WGT(us,p,maxSizeT), 
      worker, p, iters, concObj)

  /** Produce a competition linearizability tester based on the JIT Tree 
    * Search and Wing & Gong Graph Search algorithms.
    * @tparam S the type of the immutable sequential specification datatype.
    * @tparam US the type of the undoable sequential specification datatype.
    * @tparam C the type of concurrent datatypes.   
    * @param worker a function that produces a worker.
    * @param p the number of threads.
    * @param iters the number of iters performed by each worker.
    * @param concObj the concurrent object. 
    * @param s the immutable sequential specification datatype.
    * @param us the undoable sequential specification datatype.
    * @param tsLog should a timestamp-based log be used? */
  def JITWG[S, US <: Undoable, C](
    worker: WorkerType[S,US,C], p: Int, iters: Int, concObj: C, s: S, us: US, 
    tsLog: Boolean = true, maxSizeT: Long = -1, maxSizeG: Long = -1) 
  = new CompetitionTester(
      mkLog[S,US,C](tsLog, p, iters, concObj), 
      WGG(s,p,maxSizeG), JITT(us,p,maxSizeT), 
      worker, p, iters, concObj)

  /** Produce a competition linearizability tester based on the Wing & Gong
    * Tree and JIT Graph Search algorithms.
    * @tparam S the type of the immutable sequential specification datatype.
    * @tparam US the type of the undoable sequential specification datatype.
    * @tparam C the type of concurrent datatypes.   
    * @param worker a function that produces a worker.
    * @param p the number of threads.
    * @param iters the number of iters performed by each worker.
    * @param concObj the concurrent object. 
    * @param s the immutable sequential specification datatype.
    * @param us the undoable sequential specification datatype.
    * @param tsLog should a timestamp-based log be used? */
  def WGJIT[S, US <: Undoable, C](
    worker: WorkerType[S,US,C], p: Int, iters: Int, concObj: C, s: S, us: US, 
    tsLog: Boolean = true, maxSizeT: Long = -1, maxSizeG: Long = -1) 
  = new CompetitionTester(
      mkLog[S,US,C](tsLog, p, iters, concObj), 
      JITG(s,p,maxSizeG), WGT(us,p,maxSizeT), 
      worker, p, iters, concObj)

  /** Produce a competition linearizability tester based on the JIT
    * Graph and Tree Search algorithms.
    * @tparam S the type of the immutable sequential specification datatype.
    * @tparam US the type of the undoable sequential specification datatype.
    * @tparam C the type of concurrent datatypes.   
    * @param worker a function that produces a worker.
    * @param p the number of threads.
    * @param iters the number of iters performed by each worker.
    * @param concObj the concurrent object. 
    * @param s the immutable sequential specification datatype.
    * @param us the undoable sequential specification datatype.
    * @param tsLog should a timestamp-based log be used? */
  def JITJIT[S, US <: Undoable, C](
    worker: WorkerType[S,US,C], p: Int, iters: Int, concObj: C, s: S, us: US, 
    tsLog: Boolean = true, maxSizeT: Long = -1, maxSizeG: Long = -1) 
  = new CompetitionTester(
      mkLog[S,US,C](tsLog, p, iters, concObj), 
      JITG(s,p,maxSizeG), JITT(us,p,maxSizeT),
      worker, p, iters, concObj)
}
