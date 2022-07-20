package ox.cads.testing
import ox.cads.util.Profiler
import scala.collection.mutable.ArrayBuffer

/** A class performing linearizability testing on concurrent queues.
  * @tparam A the type of data stored in the queue
  * @tparam C the type of concurrent queues.
  * @param log the QueueLog object to use.
  * @param p the number of threads.
  * @param worker  a function that produces a worker, given its identity and a 
  *   log to use.
  * @param verbose a flag showing whether to give verbose output. */

class QueueLinTester[A,C]
  (log: QueueLog[A,C], p: Int, worker: QueueLinTester.WorkerType[A,C],
   verbose: Boolean = false)
{
  /** Run the tester.
    * @returns a result as defined in Solver. */   
  def apply() : Int = {
    ox.cads.util.ThreadUtil.runIndexedSystem(p, t => worker(t, log(t)))
    new QueueLinSolver(p, verbose).solve(log.getLog)
  }
}

// --------- Companion object ---------

object QueueLinTester{
  type WorkerType[A,C] = (Int, QueueThreadLog[A,C]) => Unit

  /** A queue linearizability tester. 
    * @tparam A the type of data stored in the queue.
    * @tparam C the type of concurrent queues.   
    * @param concQueue the concurrent queue. 
    * @param p the number of threads.
    * @param worker a function that produces a worker, given its identity 
    * and a log to use.
    * @param iters the number of iters performed by each worker.
    * @param tsLog should a timestamp-based log be used?
    */
  def apply[A,C](concQueue: C, p: Int, worker: WorkerType[A,C], 
		 iters: Int, tsLog: Boolean = true) 
  = {
    val log = 
      if(tsLog) new QueueTSLog[A,C](p, iters, concQueue) 
      else new SharedQueueLog[A,C](p*iters, concQueue)
    new QueueLinTester[A,C](log, p, worker)
  }

}
