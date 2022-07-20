
package ox.cads.collection

/** A trait for priority queues */
trait PriorityQueue[T]{
  /** Add item to the queue, with priority pri. */
  def add(item: T, pri: Int)

  /** Remove and return an item with minimal priority. */
  def removeMin : T
}
