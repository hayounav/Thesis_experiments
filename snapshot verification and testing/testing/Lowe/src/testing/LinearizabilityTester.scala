package ox.cads.testing

import ox.cads.util.Profiler
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Undoable

/** A tester for linearizabilty.
  * @tparam S the type of the sequential specification object.
  * @tparam C the type of the concurrent datatype.
  * @tparam L the type of log to be used.
  * @param mkLog a function to produce a log, given functions that produce 
  *   invocation and return events.
  * @param worker a function that produces a worker, given its identity and a 
  *   log to use.
  * @param p the number of workers.
  * @param iters the number of iterations to be performed by each worker.
  * @param concObj the concurrent object to use.
  * @param seqObj the sequential specification object.
  * @param solver the generic solver to use. */ 
class LinearizabilityTester[S, C, L <: GenericLog[S,C]](
  mkLog: (GenericLog.MkInvokeType[S], GenericLog.MkReturnType) => L,
  worker: LinearizabilityTester.WorkerType[S,C],
  p: Int, iters: Int, concObj: C, seqObj: S, solver: GenericSolver[S, Event])
{
  /** Run the tester.
    * @returns a result as defined in Solver. */ 
  def apply() : Int = {
    // Make log
    val log : L = mkLog(solver.mkInvoke, solver.mkReturn)
    // Run workers
    ox.cads.util.ThreadUtil.runIndexedSystem(p, i => worker(i, log(i)))
    // Test for linearizability
    solver.solve(log.getLog)
  }
}

// --------- Companion object ---------

object LinearizabilityTester{

  /** Type of functions that produce workers. */
  type WorkerType[S,C] = (Int, GenericThreadLog[S, C]) => Unit

  private def mkLog[S, C](tsLog: Boolean, p: Int, iters: Int, concObj: C)(
      mkInvoke: GenericLog.MkInvokeType[S], mkReturn: GenericLog.MkReturnType)
      : GenericLog[S,C]
  = if(tsLog) new TSLog[S,C](p, iters, concObj, mkInvoke, mkReturn)
    else new SharedLog[S,C](p*iters, concObj, mkInvoke, mkReturn)

  // --------- Factory methods ---------

  /** Produce a linearizability tester based on the Wing & Gong Graph Search
    * Algorithm and a shared log. 
    * @tparam S the type of the sequential specification datatype.
    * @tparam C the type of concurrent datatypes.   
    * @param seqObj the sequential specification datatype. 
    * @param concObj the concurrent object. 
    * @param p the number of threads.
    * @param worker a function that produces a worker.
    * @param iters the number of iters performed by each worker.
    * @param tsLog should a timestamp-based log be used? */
  def WGGraph[S, C](
      seqObj: S, concObj: C, p: Int, worker: WorkerType[S,C], 
      iters: Int, tsLog: Boolean = true, maxSize: Long = -1) 
  = new LinearizabilityTester[S, C, GenericLog[S,C]](
      mkLog(tsLog, p, iters, concObj), worker, p, iters, 
      concObj, seqObj, new WGGraph[S](seqObj, p, maxSize)
    )

  /** Produce a linearizability tester based on the Wing & Gong Tree Search
    * Algorithm and a shared log. 
    * @tparam S the type of the sequential specification datatype.
    * @tparam C the type of concurrent datatypes.   
    * @param seqObj the sequential specification datatype. 
    * @param concObj the concurrent object. 
    * @param p the number of threads.
    * @param worker a function that produces a worker.
    * @param iters the number of iters performed by each worker.
    * @param tsLog should a timestamp-based log be used? */
  def WGTree[S <: Undoable, C](
      seqObj: S, concObj: C, p: Int, worker: WorkerType[S,C], 
      iters: Int, tsLog: Boolean = true, maxSize: Long = -1) 
  = new LinearizabilityTester[S, C, GenericLog[S,C]](
      mkLog(tsLog, p, iters, concObj), worker, p, iters, 
      concObj, seqObj, new WGLinearizabilityTester[S](seqObj, p, maxSize)
    )
  
  /** Produce a linearizability tester based on JIT Tree Search and a 
    * shared log. 
    * @tparam S the type of the sequential specification datatype.
    * @tparam C the type of concurrent datatypes.   
    * @param seqObj the sequential specification datatype. 
    * @param concObj the concurrent object. 
    * @param p the number of threads.
    * @param worker a function that produces a worker.
    * @param iters the number of iters performed by each worker.
    * @param tsLog should a timestamp-based log be used? */
  def JITTree[S <: Undoable, C](
      seqObj: S, concObj: C, p: Int, worker: WorkerType[S,C], 
      iters: Int, tsLog: Boolean = true, maxSize: Long = -1)
  = new LinearizabilityTester[S, C, GenericLog[S,C]](
      mkLog(tsLog, p, iters, concObj), worker, p, iters, 
      concObj, seqObj, new JITLinUndoTester[S](seqObj, p, maxSize)
    )

  /** Produce a linearizability tester based on JIT Graph Search and a 
    * shared log. 
    * @tparam S the type of the sequential specification datatype.
    * @tparam C the type of concurrent datatypes.   
    * @param seqObj the sequential specification datatype. 
    * @param concObj the concurrent object. 
    * @param p the number of threads.
    * @param worker a function that produces a worker.
    * @param iters the number of iters performed by each worker.
    * @param tsLog should a timestamp-based log be used? */
  def JITGraph[S, C](
      seqObj: S, concObj: C, p: Int, worker: WorkerType[S,C], 
      iters: Int, tsLog: Boolean = true, maxSize: Long = -1) 
  = new LinearizabilityTester[S, C, GenericLog[S,C]](
      mkLog(tsLog, p, iters, concObj), worker, p, iters, 
      concObj, seqObj, new DFSGraphJITLinTester[S](seqObj, p, maxSize)
    )

  /** Produce a linearizability tester based on breadth-first JIT Graph 
    * Search and a shared log. 
    * @tparam S the type of the sequential specification datatype.
    * @tparam C the type of concurrent datatypes.   
    * @param seqObj the sequential specification datatype. 
    * @param concObj the concurrent object. 
    * @param p the number of threads.
    * @param worker a function that produces a worker.
    * @param iters the number of iters performed by each worker.
    * @param tsLog should a timestamp-based log be used? */
  def BFSJIT[S <: AnyRef, C](
      seqObj: S, concObj: C, p: Int, worker: WorkerType[S,C], 
      iters: Int, tsLog: Boolean = true, maxSize: Long = -1)
  = new LinearizabilityTester[S, C, GenericLog[S,C]](
      mkLog(tsLog, p, iters, concObj), worker, p, iters, 
      concObj, seqObj, new BFSJITLinTester[S](seqObj, p, maxSize)
    )

  // TODO: maybe DFSJITLin     
}
