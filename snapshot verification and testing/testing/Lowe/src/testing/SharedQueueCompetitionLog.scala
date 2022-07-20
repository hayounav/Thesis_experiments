package ox.cads.testing

/** A log, based on a shared object, used by several threads for a 
  * competition tester combining a queue tester and a generic tester.   
  * @tparam A the type of data stored in the queue.
  * @tparam S the type of the sequential specification datatype used by
  * the generic tester.
  * @tparam C the concurrent datatype. */
class SharedQueueCompetitionLog[A, S, C](
  invocs: Int, concQueue: C, 
  mkInvoke: GenericLog.MkInvokeType[S], mkReturn: GenericLog.MkReturnType)
extends QueueCompetitionLog[A, S, C]{

  /** BoundedBuffer used to store Events. */
  private val inQueue = 
    new ox.cads.collection.BoundedBuffer[(QueueLinNode,Event)](2*invocs)

  /** Internal QueueCompetitionThreadLog object used by thread t. */
  class SharedThreadLog(t: Int) extends QueueCompetitionThreadLog[A,S,C]{

    /** Log an enqueue of value by this thread.
      * @param enqueueOp the operation on the concurrent queue corresponding to
      * the enqueue, e.g. _.enqueue(value).
      * @param seqOp the corresponding operation on the sequential datatype. */
    def logEnqueue(value: A, enqueueOp: C => Unit, seqOp: S => Unit) = {
      // Create InvokeEvents, and add to log
      val invGT = mkInvoke(t, "enqueue "+value, seqOp)
      val invQT = new EnqueueInvokeEvent(t, value)
      inQueue.add((invQT, invGT))
      // perform operation
      val result = enqueueOp(concQueue)
      // log return
      val retGT = mkReturn(t, result)
      val retQT = new EnqueueReturnEvent(t)
      inQueue.add((retQT, retGT))
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
      inQueue.add((invQT, invGT))
      // perform operation
      val result = dequeueOp(concQueue)
      // log return
      val retGT = mkReturn(t, result)
      val retQT = new DequeueReturnEvent(t, result)
      inQueue.add((retQT, retGT))
      invGT.ret = retGT
    }
  }

  /** Get a GenericThreadLog object for thread t. */
  def apply(t: Int) = new SharedThreadLog(t)

  /** Get the contents of the log */
  def getLog : Array[(QueueLinNode, Event)] = inQueue.getAll

}
