package ox.cads.testing

/** A timestamp-based log used by a competition tester.   
  *
  * @tparam S1 the first sequential specification datatype. 
  * @tparam S2 the second sequential specification datatype. 
  * @tparam C the concurent datatype.
  *
  * @param p the number of workers.
  * @param iters the number of invocations by each thread.
  * @param concObj the concurrent object.
  * @param mkInvoke1 function to create the InvokeEvent corresponding to S1.
  * @param mkReturn1 function to create the ReturnEvent corresponding to S1.
  * @param mkInvoke2 function to create the InvokeEvent corresponding to S2.
  * @param mkReturn2 function to create the ReturnEvent corresponding to S2.
  */
class TSCompetitionLog[S1, S2, C](
  p: Int, iters: Int, concObj: C,
  mkInvoke1: GenericLog.MkInvokeType[S1], mkReturn1: GenericLog.MkReturnType, 
  mkInvoke2: GenericLog.MkInvokeType[S2], mkReturn2: GenericLog.MkReturnType)

extends CompetitionLog[S1, S2, C]{
  /** Array holding the individual logs. */
  private val logs = 
    Array.tabulate(p)(t => 
      new TSCompetitionThreadLog[S1,S2,C](
	t, iters, concObj, mkInvoke1, mkInvoke2, mkReturn1, mkReturn2))

  /** Get an individual log for thread t to use. */
  def apply(t: Int) : TSCompetitionThreadLog[S1,S2,C] = logs(t)

  /** Get the logs, as a single array. */
  def getLog : Array[(Event,Event)] = TS.merge(logs.map(_.get))

}
