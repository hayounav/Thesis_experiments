package ox.cads.collection
import ox.cads.locks.Lock

/** An implementation of Set based on a linked list with optimistic
  * synchronization. */
class OptimisticListSet[T] extends Set[T]{
  /** A type of nodes */
  private class Node(val item: T, val key: Int, @volatile var next: Node){
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
    if(validate(pred, curr)) (pred, curr)
    else{ pred.unlock; curr.unlock; find(item, key) } // try again
  }

  /** Check that pred is still in the list and points to curr.
    * pre: this thread has pred and curr locked. */
  private def validate(pred: Node, curr: Node) : Boolean = {
    var node = head
    while(node.key <= pred.key)
      if(node == pred) return pred.next == curr 
      else node = node.next
    false
  }

  def contains(item: T) : Boolean = {
    val key = item.hashCode
    var pred : Node = null; var curr : Node = null
    try{
      val (p, c) = find(item, key); pred = p; curr = c
      curr.key == key // linearization point
    } finally { pred.unlock; curr.unlock }
  } 

  def add(item: T): Boolean = {
    val key = item.hashCode
    var pred : Node = null; var curr : Node = null
    try{
      val (p, c) = find(item, key); pred = p; curr = c
      if(curr.key == key) false // already there; lin. point if true
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
      if(curr.key == key){ // linearization point if false
	pred.next = curr.next // linearization point
	true
      }
      else false
    } finally { pred.unlock; curr.unlock }
  } 
}
