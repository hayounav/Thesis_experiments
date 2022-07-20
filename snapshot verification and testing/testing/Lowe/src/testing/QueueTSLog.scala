package ox.cads.testing

/** A log to be used by several threads with a queue-oriented tester, 
  * based on timestamping. 
  * @tparam A the type of data stored in the queue
  * @tparam C the type of the concurrent object.
  * @param p the number of threads.
  * @param iters the number of iterations by each thread.
  * @param concQueue the shared concurrent queue. 
  */

class QueueTSLog[A, C](p:Int, iters: Int, concQueue: C)
extends QueueLog[A, C]{

  /** Array holding the individual logs. */
  private val logs = 
    Array.tabulate(p)(t => new QueueTSThreadLog[A,C](t, iters, concQueue))

  /** Get an individual log for thread t to use. */
  def apply(t: Int) : QueueTSThreadLog[A,C] = logs(t)

  /** Get the logs, as a single array. */
  def getLog : Array[QueueLinNode] = TS.merge(logs.map(_.get))


}
