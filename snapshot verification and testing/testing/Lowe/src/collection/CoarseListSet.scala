package ox.cads.collection

/** An implementation of Set based on a linked list with coarse-grained
  * synchronization. */
class CoarseListSet[T] extends ox.cads.collection.Set[T]{
  /** A type of nodes */
  private class Node(val item: T, val key: Int, var next: Node)

  /** The head and tail of the linked list. */
  private val tail = new Node(null.asInstanceOf[T], Int.MaxValue, null)
  private val head = new Node(null.asInstanceOf[T], Int.MinValue, tail)

  /** The list is ordered by key values (but with arbitrary order for Nodes
    * with the same key. */
  
  def contains(item: T) : Boolean = {
    val key = item.hashCode
    lock.mutex{ 
      var curr = head.next
      while(curr.key < key || curr.key == key && curr.item != item)
	curr = curr.next
      curr.key == key // linearization point
    }
  }

  /** Search for item.
    * @return a pair of successive nodes (pred, curr) such that:
    * (1) pred.key <= key <= curr.key; (2) if curr.key = key then 
    * curr.item = item; and (3) if item is in the list, it's in curr. 
    * pre: key = item.hashCode. */
  private def find(item: T, key: Int) : (Node, Node) = {
    var pred = head; var curr = head.next
    while(curr.key < key || curr.key == key && curr.item != item){
      pred = curr; curr = curr.next
    }
    (pred, curr)
  }

  private val lock = ox.cads.locks.Lock()

  def add(item: T) : Boolean = {
    val key = item.hashCode
    lock.mutex{
      val (pred, curr) = find(item, key)
      if(curr.key == key) false // already there
      else{
	val node = new Node(item, key, curr)
	pred.next = node // linearization point
	true
      }
    }
  }

  def remove(item: T) : Boolean = {
    val key = item.hashCode
    lock.mutex{
      val (pred, curr) = find(item, key)
      if(curr.key == key){
	pred.next = curr.next // linearization point
	true
      }
      else false
    }
  }
}

