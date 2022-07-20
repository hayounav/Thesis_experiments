
package ox.cads.collection
import ox.cads.locks.Lock

/** An unbounded total queue.  
  * Based on Herlihy & Shavit, Section 10.4. */
class UnboundedQueue[T] extends Queue[T]{
  private val enqLock = Lock() // used to protect enqueue
  private val deqLock = Lock() // used to protect dequeue

  // Nodes used in the linked list
  private class Node(val datum: T){
    @volatile var next: Node = null
    // volatile to prevent reordering in enqueue
  }
  // Head and tail of the list
  private var head = new Node(null.asInstanceOf[T]) // dummy header
  private var tail = head // last Node in list
  // There's no need for these to be volatile, as they're protected by the 
  // locks.

  /** Enqueue x */
  def enqueue(x: T) = enqLock.mutex{
    val e = new Node(x); tail.next = e; tail = e 
    // linearization point is update to tail.next
  }

  /** Optionally dequeue.  Return None of the queue is empty. */
  def dequeue : Option[T] = deqLock.mutex{
    if(head.next == null) None
    else{ head = head.next; Some(head.datum) }
  }
}
