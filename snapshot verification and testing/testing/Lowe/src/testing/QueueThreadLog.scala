package ox.cads.testing

/** A log to be used by a single thread with a queue-oriented tester. 
  * @tparam A the type of data stored in the queue.
  * @tparam C the type of the concurrent object. */

abstract class QueueThreadLog[A, C]{

  /** Log an enqueue operation of value by the thread.
    * @param enqueueOp the operation on the concurrent queue corresponding to
    * the enqueue, e.g. _.enqueue(value). */
  def logEnqueue(value: A, enqueueOp: C => Unit)

  /** Log a dequeue operation by the thread.
    * @param dequeueOp the operation on the concurrent queue corresponding to
    * the dequeue, e.g. _.dequeue. */
  def logDequeue(dequeueOp: C => Option[A])


}
