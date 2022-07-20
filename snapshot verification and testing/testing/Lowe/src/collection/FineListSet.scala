package ox.cads.collection

/** An implementation of Set based on a linked list with fine-grained
  * synchronization. */
class FineListSet[T] extends Set[T]{
  /** A type of nodes */
  private class Node(val item: T, val key: Int, var next: Node){
    private val l = ox.cads.locks.Lock.FairLock
    def lock = l.lock
    def unlock = l.unlock
  }

  /** The head and tail of the linked list, linked to the tail */
  private val tail = new Node(null.asInstanceOf[T], Int.MaxValue, null)
  private val head = new Node(null.asInstanceOf[T], Int.MinValue, tail)

  def contains(item: T) : Boolean = {
    val key = item.hashCode
    head.lock; var curr = head.next; curr.lock
    try{
      head.unlock
      while(curr.key < key || curr.key == key & curr.item != item){
	val n = curr.next; n.lock; curr.unlock; curr = n
	// val n = curr.next; curr.unlock; n.lock; curr = n
      }
      curr.key == key // linearization point
    } finally curr.unlock
  }

  /** Search for item.
    * @return a pair of successive nodes (pred, curr) such that:
    * (1) pred.key <= key <= curr.key; (2) if curr.key = key then 
    * curr.item = item; and (3) if item is in the list, it's in curr.
    * This thread will have pred and curr locked on return.
    * pre: key = item.hashCode. */
  private def find(item: T, key: Int) : (Node, Node) = {
    var pred = head; pred.lock
    var curr = head.next; curr.lock
    while(curr.key < key || curr.key == key & curr.item != item){
      pred.unlock; pred = curr; curr = curr.next; curr.lock
    }
    assert(pred != null && curr != null)
    (pred, curr)
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
    // try{
      val (p, c) = find(item, key); pred = p; curr = c
      if(curr.key == key){
	pred.next = curr.next // linearization point
	pred.unlock; curr.unlock; true
      }
      else{ pred.unlock; curr.unlock; false }
    // } finally { pred.unlock; curr.unlock }
  } 
}
