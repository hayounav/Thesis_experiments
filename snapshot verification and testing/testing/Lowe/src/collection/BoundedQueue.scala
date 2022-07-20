package ox.cads.collection
import ox.cads.locks.Lock
import java.util.concurrent.atomic.AtomicInteger

/** A bounded partial queue.  
  * Based on Herlihy & Shavit, Section 10.3.
  * @param capacity the capacity of the queue. */
class BoundedQueue[T](capacity: Int) extends PartialQueue[T]{
  private val enqLock = Lock() // used to protect enqueue
  private val deqLock = Lock() // used to protect dequeue
  private val notFull = enqLock.newCondition // used to wait until not full
  private val notEmpty = deqLock.newCondition // used to wait until not empty
  private val size = new AtomicInteger(0) // current # elements
  
  // Nodes used in the linked list
  private class Node(val datum: T){
    @volatile var next: Node = null
  }
  private var head = new Node(null.asInstanceOf[T]) // dummy header
  private var tail = head // last Node in list

  /** Enqueue x in the queue.  Block while the queue is full. */
  def enqueue(x: T) = {
    var mustWakeDequeuers = false // must we wake up sleeping dequeuers?
    val node = new Node(x)
    enqLock.mutex{
      notFull.await(size.get < capacity)
      tail.next = node // linearization point
      tail = node
      if(size.getAndIncrement == 0) mustWakeDequeuers = true      
    }
    if(mustWakeDequeuers)
      // need to obtain deqLock before signalling to avoid lost wake-up
      deqLock.mutex{ notEmpty.signalAll }
  }

  /** Dequeue an item from the queue.  Block while the queue is empty. */
  def dequeue : T = {
    var result = null.asInstanceOf[T] // will hold the result
    var mustWakeEnqueuers = false // must we wake up sleeping enqueuers?
    deqLock.mutex{
      notEmpty.await(size.get > 0)
      result = head.next.datum // value to return
      head = head.next // remove item
      if(size.getAndDecrement == capacity) mustWakeEnqueuers = true
    }
    if(mustWakeEnqueuers) enqLock.mutex{ notFull.signalAll }
    result
  }

}
