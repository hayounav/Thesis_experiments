package ox.cads.locks

/** A queue lock, suitable for an arbitrary number of threads.
  *
  * Based on Herlihy & Shavit, Section 7.5.2. 
  */
class CLHLock extends Lock{
  /** Threads wait on objects of the following type. */
  private class QNode{
    @volatile var locked = false
  }

  /** Node on which the local thread signals it was unlocked. */
  private val myNode = new ThreadLocal[QNode]{
    override def initialValue() = new QNode
  }
  /** Node on which the local thread waits to acquire the lock. */
  private val myPred = new ThreadLocal[QNode]{
    override def initialValue() = null : QNode
  }

  /** The last node in the queue; the next thread attempting a lock 
    * should wait on this. */
  private val tail = 
    new java.util.concurrent.atomic.AtomicReference[QNode](new QNode)

  def lock = {
    // Get node to wait on; and return my thread to tail. 
    val qnode = myNode.get; qnode.locked = true
    val pred = tail.getAndSet(qnode)
    // Wait for signal.
    myPred.set(pred)
    while(pred.locked){ }
  }

  def unlock = {
    // Signal to next thread.
    val qnode = myNode.get; qnode.locked = false
    // Store myPred for next time.
    myNode.set(myPred.get)
  }

  def tryLock = ???
}
