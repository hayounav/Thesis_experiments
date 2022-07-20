package ox.cads.collection

import java.util.concurrent.atomic.AtomicReferenceArray
import ox.cads.util.ThreadID

/** A lock-based concurrent skiplist.
  * Based on Herlihy & Shavit, Section 14.3. */
class CollisionResistantLazySkipList[T] extends Set[T]{
  private val NumLevels = 32 // Maximum level of the lists
  private val MaxLevel = NumLevels-1 // Maximum level number

  /** A node, holding item with hash key, included in the lists up to 
    * level height. */ 
  private class Node(val item: T, val key: Int, height: Int){
    // constructor for sentinels
    def this(key: Int) = this(null.asInstanceOf[T], key, NumLevels)
    val next = new AtomicReferenceArray[Node](height) // next nodes in lists
    @volatile var marked = false // is this marked for deletion?
    @volatile var fullyLinked = false // is this fully linked into the list?
    val topLevel = height-1 // top level this appears in
    // Locking and unlocking of this node
    private val myLock = new ox.cads.locks.SimpleReentrantLock
    def lock = myLock.lock
    def unlock = myLock.unlock
  }

  // Head and tail sentinels. 
  private val head = new Node(Int.MinValue)
  private val tail = new Node(Int.MaxValue)
  for(i <- 0 until NumLevels) head.next.set(i, tail)

  /** Search for item and key.
    * @return a pair of successive nodes (lFound, preds, succs) such that
    * lFound is the highest level where item appears, or -1 if item does not
    * appear; and for each level i:
    * (1) preds(i).key <= key <= succs(i).key; 
    * (2) if succs(i).key = key then succs(i).item = item; and 
    * (3) if item is in the list at level (i), it's in succs(i); otherwise
    *     preds(i).key < key.
    * pre: key = item.hashCode. */
  private def find(item: T, key: Int) : (Int, Array[Node], Array[Node]) = {
    // Predecessors and successors at each level
    val preds, succs = new Array[Node](NumLevels)
    var lFound = -1 // level at which item was found
    var pred = head
    for(level <- MaxLevel to 0 by -1){
      var curr = pred.next.get(level)
      while(curr.key < key){ pred = curr; curr = pred.next.get(level) }
      // Now scan the items matching key to see if item appears
      var pred1 = pred; var curr1 = curr
      while(curr1.key == key && curr1.item != item){
	pred1 = curr1; curr1 = curr1.next.get(level)
      }
      if(curr1.key == key){ 
	assert(curr1.item == item); pred = pred1; curr = curr1
	if(lFound == -1) lFound = level
      }
      preds(level) = pred; succs(level) = curr
    }
    (lFound, preds, succs)
  }
      
  /** Does the set contain x? */
  def contains(x: T) = {
    val (lFound, preds, succs) = find(x, x.hashCode)
    lFound != -1 && succs(lFound).fullyLinked && !succs(lFound).marked
  }

  // conditional probability of node at level i also being at level i+1 
  // (for i < NumLevels - 1).
  private val p = 0.5

  /** Choose a level at which to insert an item. */
  private def randomLevel : Int = {
    var i = 0
    while(i < MaxLevel && scala.util.Random.nextFloat < p) i += 1
    i
  }

  /** Add x to the set.
    * @return true iff x was not previously in the set. */
  def add(x: T) : Boolean = {
    val key = x.hashCode
    val topLevel = randomLevel // level to insert at
    while(true){
      val (lFound, preds, succs) = find(x, key)
      if(lFound != -1){ // x is already there
	val node = succs(lFound); assert(node.item == x)
	if(!node.marked){ // x in set
	  // wait until fully linked; exercise: why?
	  while(!node.fullyLinked){ }; return false
	}
	// else retry
      }
      else{
	// Try to lock preds[0..topLevel], performing validation
	var highestLocked = -1 // highest indexed lock locked
	try{
	  var valid = true; var level = 0
	  while(valid && level <= topLevel){
	    val pred = preds(level); val succ = succs(level)
	    pred.lock; highestLocked = level
	    valid = !pred.marked && !succ.marked && 
	      pred.next.get(level) == succ
	    level += 1
	  }
	  if(valid){
	    // Create new node and link it in
	    val newNode = new Node(x, key, topLevel+1)
	    for(level <- 0 to topLevel) newNode.next.set(level, succs(level))
	    for(level <- 0 to topLevel) preds(level).next.set(level, newNode)
	    newNode.fullyLinked = true // linearization point
	    return true
	  } // if validation failed, retry
	} finally{ for(level <- 0 to highestLocked) preds(level).unlock }
      } // end of else
    } // end of while
    sys.error("unreachable")
  }

  /** Remove x from the set. */
  def remove(x: T) : Boolean = {
    val (lFound, preds, succs) = find(x, x.hashCode)
    if(lFound == -1) return false // not there
    val victim = succs(lFound) // node to remove
    if(victim.fullyLinked && victim.topLevel == lFound && !victim.marked){
      victim.lock
      if(victim.marked){ victim.unlock; return false } // already deleted
      victim.marked = true // linearization point
      disconnect(x, victim, preds); true
    } // end of if
    else false
  }  

  /** Physically disconnect the node victim with value x; preds are its
    * predecessors.
    * Pre: victim is locked. */
  private def disconnect(x: T, victim: Node, preds: Array[Node]) : Unit = {
    val topLevel = victim.topLevel; var highestLocked = -1
    try{
      // Try to lock preds[0..topLevel], performing validation
      var valid = true; var level = 0
      while(valid && level <= topLevel){
	val pred = preds(level); pred.lock; highestLocked = level
	valid = !pred.marked && pred.next.get(level) == victim
	level += 1
      }
      if(valid){ // Now physically disconnect
	for(level <- topLevel to 0 by -1)
	  preds(level).next.set(level, victim.next.get(level))
	victim.unlock; return
      } 
    } finally{ for(level <- 0 to highestLocked) preds(level).unlock }  
    // Validation failed; retry
    val (lFound, preds1, succs) = find(x, x.hashCode)
    assert(lFound == topLevel, lFound+"; "+topLevel)
    assert(succs(topLevel) == victim)
    disconnect(x, victim, preds1)
  }

  /** Remove x from the set. 
    * This is the version in the book.  I think the above is clearer. */
  private def removeX(x: T) : Boolean = {
    var victim : Node = null // node to delete
    var isMarked = false // have we yet marked victim?
    var topLevel = -1 // topLevel of victim
    while(true){
      val (lFound, preds, succs) = find(x, x.hashCode)
      if(lFound != -1) victim = succs(lFound)
      if(!isMarked && lFound != -1 && victim.fullyLinked && 
	 victim.topLevel == lFound && !victim.marked){
	   topLevel = victim.topLevel
	   victim.lock
	   if(victim.marked){ victim.unlock; return false } // already deleted
	   victim.marked = true; isMarked = true // linearization point
	 }
      if(isMarked){
	// Try to lock preds[0..topLevel], performing validation
	var highestLocked = -1
	try{
	  var valid = true; var level = 0
	  while(valid && level <= topLevel){
	    val pred = preds(level); pred.lock; highestLocked = level
	    valid = !pred.marked && pred.next.get(level) == victim
	    level += 1
	  }
	  if(valid){
	    for(level <- topLevel to 0 by -1)
	      preds(level).next.set(level, victim.next.get(level))
	    victim.unlock; return true
	  } // if validation failed, retry
	} finally{ for(level <- 0 to highestLocked) preds(level).unlock }  
      } // end of if(isMarked)
      else return false
    }
    sys.error("unreachable")
  }
}
