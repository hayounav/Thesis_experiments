package ox.cads.collection

/** A trait for partial queues.
  *
  * Partial queues will block on a dequeue from an empty queue or an enqueue on
  * a full queue.  Total queues will return None on a dequeue from an empty
  * queue. */
trait PartialQueue[T]{
  /** Enqueue value, blocking if the queue is full. */
  def enqueue(value: T) : Unit 

  /** Dequeue and return the first item in the queue. */
  def dequeue : T
}
