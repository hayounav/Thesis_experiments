/** An implementation of Set based on a linked list with lazy
  * synchronization. */

package ox.cads.collection
import ox.cads.locks.Lock

class LazyListSet[T] extends Set[T]{
  /** A type of nodes */
  private class Node(val item: T, val key: Int, @volatile var next: Node){
    @volatile var marked = false
    private val l = Lock()
    def lock = l.lock
    def unlock = l.unlock
  }

  /** The head and tail of the linked list, linked to the tail */
  private val tail = new Node(null.asInstanceOf[T], Int.MaxValue, null)
  private val head = new Node(null.asInstanceOf[T], Int.MinValue, tail)

  /** Search for item.
    * @return a pair of successive nodes (pred, curr) such that:
    * (1) pred.key <= key <= curr.key; (2) if curr.key = key then 
    * curr.item = item; and (3) if item is in the list, it's in curr.
    * This thread will have pred and curr locked on return.
    * pre: key = item.hashCode. */
  private def find(item: T, key: Int) : (Node, Node) = {
    // optimistically search for item
    var pred = head; var curr = head.next
    while(curr.key < key || curr.key == key & curr.item != item){
      pred = curr; curr = curr.next
    }
    // lock then validate
    pred.lock; curr.lock
    if(validate(pred, curr)) return (pred, curr)
    else{ pred.unlock; curr.unlock; find(item, key) } // try again
  }  

  /** Validate pred and curr: that neither is marked and that pred points 
    * to curr. */
  private def validate(pred: Node, curr: Node) : Boolean = 
    !pred.marked && !curr.marked && pred.next == curr

  def contains(item: T) : Boolean = {
    val key = item.hashCode; var curr = head.next
    while(curr.key < key || curr.key == key & curr.item != item)
      curr = curr.next
    curr.key == key && !curr.marked // linearization point
  }

  def add(item: T): Boolean = {
    val key = item.hashCode
    var pred : Node = null; var curr : Node = null
    try{
      val (p, c) = find(item, key); pred = p; curr = c
      if(curr.key == key) false // already there
      else{
	val node = new Node(item, key, curr)
	pred.next = node // linearization point
	true
      }
    } finally { pred.unlock; curr.unlock }
  } 

  def remove(item: T): Boolean = {
    val key = item.hashCode
    var pred : Node = null; var curr : Node = null
    try{
      val (p, c) = find(item, key); pred = p; curr = c
      if(curr.key == key){
	curr.marked = true // logically remove item; linearization point
	pred.next = curr.next // physically remove item
	true
      }
      else false
    } finally { pred.unlock; curr.unlock }
  } 
}
