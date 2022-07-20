package ox.cads.collection

/** A trait for total queues. */
trait Queue[T]{
  /** Enqueue value, maybe blocking if the queue is full. */
  def enqueue(value: T) : Unit 

  /** Dequeue and return the first item in the queue, or return None if the 
    * queue is empty. */
  def dequeue : Option[T]
}
