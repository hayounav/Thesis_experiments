package ox.cads.testing

/** A log to be used by several threads with a queue-oriented tester.
  * @tparam A the type of data stored in the queue
  * @tparam C the type of the concurrent object. */

abstract class QueueLog[A,C]{

  /** Get a log for thread t to use. */
  def apply(t: Int) : QueueThreadLog[A, C]

  /** Get the contents of the log, as a single array of events. */
  def getLog: Array[QueueLinNode]

}
