package ox.cads.testing

/** A log, based on a timestamping, used by several threads for a 
  * competition tester combining a queue tester and a generic tester.   
  * @tparam A the type of data stored in the queue.
  * @tparam S the type of the sequential specification datatype used by
  * the generic tester.
  * @tparam C the concurrent datatype. */
class TSQueueCompetitionLog[A, S, C](
  p: Int, iters: Int, concQueue: C, 
  mkInvoke: GenericLog.MkInvokeType[S], mkReturn: GenericLog.MkReturnType)
extends QueueCompetitionLog[A, S, C]{

  /** Array holding the individual logs. */
  private val logs = 
    Array.tabulate(p)(t => 
      new TSQueueCompetitionThreadLog[A,S,C](
	t, iters, concQueue, mkInvoke, mkReturn))

  /** Get an individual log for thread t to use. */
  def apply(t: Int) : TSQueueCompetitionThreadLog[A,S,C] = logs(t)

  /** Get the logs, as a single array. */
  def getLog : Array[(QueueLinNode,Event)] = TS.merge(logs.map(_.get))

}
