package ox.cads.testing

/** A log used by several threads for a competition tester combining a
  * queue tester and a generic tester.   
  * @tparam A the type of data stored in the queue.
  * @tparam S the type of the sequential specification datatype used by
  * the generic tester.
  * @tparam C the concurent datatype. */
abstract class QueueCompetitionLog[A, S, C]{
  /** Get a log for thread t to use. */
  def apply(t: Int) : QueueCompetitionThreadLog[A, S, C]

  /** Get the contents of the log, as a single array of events. */
  def getLog : Array[(QueueLinNode, Event)]
}
