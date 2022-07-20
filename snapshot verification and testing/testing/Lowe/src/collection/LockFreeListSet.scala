/** An lock-free implementation of Set based on a linked list. */

package ox.cads.collection
import ox.cads.atomic.AtomicPair

class LockFreeListSet[T] extends Set[T]{
  /** A type of nodes */
  private class Node(val item: T, val key: Int, n: Node){
    val nextMark = new AtomicPair[Node,Boolean](n, false)
    def next = nextMark.getFirst
    def marked = nextMark.getSecond
  }

  /** The head and tail of the linked list, linked to the tail */
  private val tail = new Node(null.asInstanceOf[T], Int.MaxValue, null)
  private val head = new Node(null.asInstanceOf[T], Int.MinValue, tail)

  /** Search for item.
    * @return a pair of successive nodes (pred, curr) such that:
    * (1) pred.key <= key <= curr.key; (2) if curr.key = key then 
    * curr.item = item; and (3) if item is in the list, it's in curr.
    * Also remove any marked nodes encountered. 
    * pre: key = item.hashCode. */
  private def find(item: T, key: Int) : (Node, Node) = {
    var pred = head; var curr = pred.next; var succ : Node = null
    while(true){
      var done = false
      // remove consecutive marked nodes from curr onwards
      do{
	val (s, marked) = curr.nextMark.get; succ = s
	if(marked){
	  val ok = pred.nextMark.compareAndSet((curr,false), (succ,false))
	  if(!ok) return find(item, key) // re-start
	  else curr = succ
	}
	else done = true
      } while(!done)
      // have we found item?
      if(curr.key > key || curr.key == key && curr.item == item)
	return (pred, curr)
      else{ pred = curr; curr = succ } // advance
    }
    sys.error("unreachable")
  }

  def contains(item: T) : Boolean = {
    val key = item.hashCode
    var curr = head.next
    while(curr.key < key || curr.key == key & curr.item != item)
      curr = curr.next
    curr.key == key && !curr.marked
  }

  def add(item: T) : Boolean = {
    val key = item.hashCode; var done = false
    do{
      val (pred, curr) = find(item, key)
      if(curr.key == key) return false // already there
      else{
	val node = new Node(item, key, curr)
	done = pred.nextMark.compareAndSet((curr, false), (node, false))
      }
    } while(!done) // else try again
    true
  }

  def remove(item: T) : Boolean = {
    val key = item.hashCode; var done = false
    do{
      val (pred, curr) = find(item, key)
      if(curr.key != key) return false // not there
      else{
	val succ = curr.next
	// mark curr to logically delete it
	done = curr.nextMark.compareAndSet((succ, false), (succ, true))
	if(done){
	  // try to advance pred to tidy up
	  pred.nextMark.compareAndSet((curr, false), (succ, false))
	}
      }
    } while(!done)
    true
  }

}
