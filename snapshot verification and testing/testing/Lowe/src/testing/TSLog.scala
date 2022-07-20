package ox.cads.testing

/** A log to be used by several threads, based on timestamping.
  * @tparam S the type of the sequential specification object. 
  * @tparam C the type of the concurrent object. 
  * @param p the number of threads.
  * @param iters the number of invocations for each thread.
  * @param concObj the concurrent object.
  * @param mkInvoke function to create an InvokeEvent.
  * @param mkReturn function to create a new ReturnEvent.
*/
class TSLog[S,C](
  p: Int, iters: Int, concObj: C,
  mkInvoke: GenericLog.MkInvokeType[S], mkReturn: GenericLog.MkReturnType)
extends GenericLog[S,C]{
  /** Array holding the individual logs. */
  private val logs = 
    Array.tabulate(p)(t => 
      new TSThreadLog[S,C](t, iters, concObj, mkInvoke, mkReturn))

  /** Get an individual log for thread t to use. */
  def apply(t: Int) : TSThreadLog[S,C] = logs(t)

  /** Get the logs, as a single array. */
  def getLog : Array[Event] = TS.merge(logs.map(_.get))
}
		  
