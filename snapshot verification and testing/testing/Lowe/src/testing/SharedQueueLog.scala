package ox.cads.testing

/** A log to be used by several threads, with a queue-oriented tester, 
  * based on a shared array.
  * Note that logging involves a synchronization event, which will have an 
  * effect upon the caches of threads, thereby possibly missing Java Memory
  * Model-based errors. 
  * @tparam A the type of data stored in the queue
  * @tparam C the type of the concurrent object. 
  * @param invocs the number of invocations to be logged
  * @param concQueue the concurrent queue object.
  */

class SharedQueueLog[A, C](invocs: Int, concQueue: C)
extends QueueLog[A, C]{
  /** BoundedBuffer used to store Events. */
  private val inQueue = 
    new ox.cads.collection.BoundedBuffer[QueueLinNode](2*invocs)

  /** Internal GenericThreadLog object. */
  class SharedThreadLog(t: Int) extends QueueThreadLog[A,C]{
    /** Log an enqueue operation of value by the thread.
      * @param enqueueOp the operation on the concurrent queue corresponding to
      * the enqueue, e.g. _.enqueue(value). */
    def logEnqueue(value: A, enqueueOp: C => Unit) = {
      // log invocation
      val e = new EnqueueInvokeEvent(t, value); inQueue.add(e)
      // perform operation
      val result = enqueueOp(concQueue)
      // log return
      val e1 = new EnqueueReturnEvent(t); inQueue.add(e1)
    }
    
    /** Log a dequeue operation by the thread.
      * @param dequeueOp the operation on the concurrent queue corresponding to
      * the dequeue, e.g. _.dequeue. */
    def logDequeue(dequeueOp: C => Option[A]) = {
      // log invocation
      val e = new DequeueInvokeEvent(t); inQueue.add(e)
      // perform operation
      val result : Option[A] = dequeueOp(concQueue)
      // log return
      val e1 = new DequeueReturnEvent(t, result); inQueue.add(e1)
    }
  }

  /** Get a GenericThreadLog object for thread t. */
  def apply(t: Int) = new SharedThreadLog(t)

  /** Get the contents of the log */
  def getLog : Array[QueueLinNode] = inQueue.getAll

}
