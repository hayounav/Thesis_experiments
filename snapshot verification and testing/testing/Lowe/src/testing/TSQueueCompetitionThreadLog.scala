package ox.cads.testing

/** A log to be used by a single thread for a competition tester using a 
  * queue-oriented solver and a generic solver.  
  * @tparam A the type of data stored in the queue.
  * @tparam S the sequential specification datatype.  
  * @tparam C the concurrent queue.
  *
  * @param t the identity of this worker.
  * @param iters the number of invocations by each thread.
  * @param concQueue the concurrent object.
  * @param mkInvoke function to create the InvokeEvent corresponding to S.
  * @param mkReturn function to create the ReturnEvent corresponding to S. */
class TSQueueCompetitionThreadLog[A, S, C](
  t: Int, iters: Int, concQueue: C,
  mkInvoke: GenericLog.MkInvokeType[S], mkReturn: GenericLog.MkReturnType)
extends QueueCompetitionThreadLog[A, S, C]{

  /** Array holding the events. */
  private val events = new Array[TS[(QueueLinNode,Event)]](2*iters)
		 
  /** Index of next free slot in events. */
  private var index = 0

  /** Log an enqueue operation of value by the thread.
    * @param value the value enqueued.
    * @param enqueueOp the operation on the concurrent queue corresponding to
    * the enqueue, e.g. _.enqueue(value). 
    * @param seqOp the corresponding operation on the sequential 
    * specification datatype. */
  def logEnqueue(value: A, enqueueOp: C => Unit, seqOp: S => Unit) = {
    // Create InvokeEvents, and add to log
    val invGT = mkInvoke(t, "enqueue "+value, seqOp)
    val invQT = new EnqueueInvokeEvent(t, value)
    events(index) = new TS((invQT, invGT)); index += 1
    // perform operation
    val result = enqueueOp(concQueue)
    // log return
    val retGT = mkReturn(t, result)
    val retQT = new EnqueueReturnEvent(t)
    events(index) = new TS((retQT, retGT)); index += 1
    invGT.ret = retGT
  }

  /** Log a dequeue operation by this thread.
   * @param dequeueOp the operation on the concurrent queue corresponding to
   * the dequeue, e.g. _.dequeue.
   * @param seqOp the corresponding operation on the sequential datatype. */
  def logDequeue(dequeueOp: C => Option[A], seqOp: S => Option[A]) = {
    // Create InvokeEvents, and add to log
    val invGT = mkInvoke(t, "dequeue ", seqOp)
    val invQT = new DequeueInvokeEvent(t)
    events(index) = new TS((invQT, invGT)); index += 1
    // perform operation
    val result = dequeueOp(concQueue)
    // log return
    val retGT = mkReturn(t, result)
    val retQT = new DequeueReturnEvent(t, result)
    events(index) = new TS((retQT, retGT)); index += 1
    invGT.ret = retGT
  }

  /** Get the log. */
  protected[testing] def get : Array[TS[(QueueLinNode,Event)]] = events
}
