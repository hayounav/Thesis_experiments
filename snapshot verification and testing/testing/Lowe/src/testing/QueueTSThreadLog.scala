package ox.cads.testing

/** A log to be used by a single thread with a queue-oriented tester, 
  * based on timestamping. 
  * @tparam A the type of data stored in the queue
  * @tparam C the type of the concurrent object.
  * @param t the identity of this thread.
  * @param iters the number of operations performed.
  * @param concQueue the concurrent queue.
*/ 

class QueueTSThreadLog[A, C](t: Int, iters: Int, concQueue: C)
extends QueueThreadLog[A, C]{
  /** Array holding the events. */
  private val events = new Array[TS[QueueLinNode]](2*iters)
		 
  /** Index of next free slot in events. */
  private var index = 0

  /** Log an enqueue operation of value by the thread.
    * @param enqueueOp the operation on the concurrent queue corresponding to
    * the enqueue, e.g. _.enqueue(value). */
  def logEnqueue(value: A, enqueueOp: C => Unit) = {
    // log invocation
    val e = new EnqueueInvokeEvent(t, value)
    events(index) = new TS(e); index += 1
    // perform operation
    val result = enqueueOp(concQueue)
    // log return
    val e1 = new EnqueueReturnEvent(t)
    events(index) = new TS(e1); index += 1
  }

  /** Log a dequeue operation by the thread.
    * @param dequeueOp the operation on the concurrent queue corresponding to
    * the dequeue, e.g. _.dequeue. */
  def logDequeue(dequeueOp: C => Option[A]) = {
    // log invocation
    val e = new DequeueInvokeEvent(t)
    events(index) = new TS(e); index += 1
    // perform operation
    val result : Option[A] = dequeueOp(concQueue)
    // log return
    val e1 = new DequeueReturnEvent(t, result)
    events(index) = new TS(e1); index += 1
  }

  /** Get the log. */
  protected[testing] def get : Array[TS[QueueLinNode]] = events
}
