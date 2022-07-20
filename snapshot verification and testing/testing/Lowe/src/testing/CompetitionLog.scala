package ox.cads.testing
import scala.collection.mutable.Undoable

/** A log used by a competition tester.   
  * @tparam S1 the first sequential specification datatype. 
  * @tparam S2 the second sequential specification datatype. 
  * @tparam C the concurent datatype. */
trait CompetitionLog[S1, S2, C]{

  /** Get a log for thread t to use. */
  def apply(t: Int) : CompetitionThreadLog[S1, S2, C]

  /** Get the contents of the log, as a single array of events. */
  def getLog : Array[(Event, Event)]
}
