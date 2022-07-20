package ox.cads.testing

/** A log to be used by a single thread for a cometition tester combining
  * a queue tester and a generic tester.  
  * @tparam A the type of data stored in the queue.
  * @tparam S the type of the sequential specification object used by 
  * the generic tester. 
  * @tparam C the type of the concurrent object. */
abstract class QueueCompetitionThreadLog[A, S, C]{

  /** Log an enqueue operation of value by the thread.
    * @param value the value enqueued.
    * @param enqueueOp the operation on the concurrent queue corresponding to
    * the enqueue, e.g. _.enqueue(value). 
    * @param seqOp the corresponding operation on the sequential 
    * specification datatype. */
  def logEnqueue(value: A, enqueueOp: C => Unit, seqOp: S => Unit)

  /** Log a dequeue operation by the thread.
    * @param dequeueOp the operation on the concurrent queue corresponding to
    * the dequeue, e.g. _.dequeue.
    * @param seqOp the corresponding operation on the sequential 
    * specification datatype. */
  def logDequeue(dequeueOp: C => Option[A], seqOp: S => Option[A])

}
