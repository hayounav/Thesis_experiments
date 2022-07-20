package ox.cads.collection

import ox.cads.atomic.AtomicPair

/** A set based on a lock-free skiplist.
  * Based on Herlihy & Shavit, Section 14.4. 
  * This aims to be correct even with collisions of hash codes. */
class CollisionResistantLockFreeSkipList[T] extends Set[T]{
  private val NumLevels = 32 // Maximum level of the lists
  private val MaxLevel = NumLevels-1 // Maximum level number

  /** A node, holding item with hash key, included in the lists up to 
    * level height. */ 
  private class Node(val item: T, val key: Int, height: Int){
    val topLevel = height-1 // top level this appears in
    // constructor for sentinels
    def this(key: Int) = this(null.asInstanceOf[T], key, NumLevels)
    // next nodes in lists, together with marks
    var nextMark = Array.fill(height)(new AtomicPair[Node,Boolean](null,false))
    def next(level: Int) = nextMark(level).getFirst
    def mark(level: Int) = nextMark(level).getSecond
  }

  // Head and tail sentinels. 
  private val head = new Node(Int.MinValue)
  private val tail = new Node(Int.MaxValue)
  for(i <- 0 until NumLevels) head.nextMark(i).set(tail,false)

  /** Does the set contain x? */
  def contains(x: T): Boolean = {
    val key = x.hashCode; var pred = head; var curr : Node = null
    for(level <- MaxLevel to 0 by -1){
      curr = pred.next(level); var done = false
      do{
	// Find next unmarked node
	while(curr.mark(level)) curr = curr.next(level)
	// Scan past nodes with smaller key
	if(curr.key < key){ pred = curr; curr = curr.next(level) }
	else done = true
      } while(!done)
    } // end of for
    // Now scan for x
    while(curr.key == key && curr.item != x) curr = curr.next(0)
    curr.key == key && !curr.mark(0)
  }

  /** Search for item and key.
    * Also remove nodes marked for deletion.
    * Pre: key = item.hashCode.
    * @return a triple (found, preds, succs), where found says whether item 
    * was found, and preds and succs give the nodes before and after item
    * at each level, or where item should be inserted. */
  private def find(item: T, key: Int) : (Boolean, Array[Node], Array[Node]) = {
    // Predecessors and successors at each level
    val preds, succs = new Array[Node](NumLevels)
    var pred = head
    for(level <- MaxLevel to 0 by -1){
      var curr = pred.next(level); var found = false
      // Scan over nodes with smaller key
      do{
	// remove consecutive marked nodes from curr onwards
	curr = removeMarked(level, pred, curr)
	if(curr == null) return find(item, key)// removeMarked failed; re-start
       	if(curr.key < key){ pred = curr; curr = curr.next(level) }
       	else found = true
      } while(!found)
      // Now scan for item
      var pred1 = pred; var curr1 = curr; found = false
      do{
	curr1 = removeMarked(level, pred1, curr1)
	if(curr1 == null) return find(item, key) // re-start
	if(curr1.key == key && curr1.item != item){ 
	  pred1 = curr1; curr1 = curr1.next(level) 
	}
       	else found = true
      } while(!found)
      if(curr1.key == key){ 
	assert(curr1.item == item); preds(level) = pred1; succs(level) = curr1
      }
      else{ preds(level) = pred; succs(level) = curr }
      // In either case, start search at next level from pred.
    } // end of for loop
    (succs(0).key == key && succs(0).item == item, preds, succs)
  }

  /** Remove consecutive marked nodes at level level from curr onwards, 
    * returning the first unmarked node, or null if this fails.  
    * pred is expected to be the predecessor of curr. */
  private def removeMarked(level: Int, pred: Node, curr: Node) : Node = {
    var curr1 = curr
    while(true){
      val (succ, marked) = curr1.nextMark(level).get
      if(marked){
	if(! pred.nextMark(level).compareAndSet((curr1, false), (succ,false)))
	  return null // signal re-start
	else curr1 = succ
      }
      else return curr1
    }
    sys.error("unreachable")
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

  /** Add x to the set. */
  def add(x: T): Boolean = {
    val key = x.hashCode
    var preds, succs : Array[Node] = null
    val topLevel = randomLevel // level to insert at
    while(true){
      val (found, ps, ss) = find(x, key); preds = ps; succs = ss
      if(found) return false // already there
      // Create new node, linked to succs
      val newNode = new Node(x, key, topLevel+1)
      for(level <- 0 to topLevel)
	newNode.nextMark(level).set(succs(level), false)
      // Link at bottom level; if the CAS succeeds, it is the lin. point
      val pred = preds(0); val succ = succs(0)
      if(pred.nextMark(0).compareAndSet((succ, false), (newNode, false))){
	// Link at other levels
	for(level <- 1 to topLevel){
	  var done = false
	  do{
	    val pred = preds(level); val succ = succs(level)
	    if(pred.nextMark(level).compareAndSet((succ, false), 
						  (newNode, false)))
	      done = true
	    else{ 
	      // renew preds, succs, then try again at this level
	      val (_, ps, ss) = find(x, key); preds = ps; succs = ss 
	    }
	  } while(!done)
	} // end of for loop
	return true
      }
      // If CAS at level 0 fails, retry
    } // end of while(true)
    sys.error("unreachable")
  }

  /** Remove x from the set. */
  def remove(x: T): Boolean = {
    val key = x.hashCode
    val (found, preds, succs) = find(x, key)
    if(!found) return false // it's not there
    val victim = succs(0) // node to remove
    assert(victim.item == x)
    for(level <- victim.topLevel to 1 by -1){
      // Mark victim at this level
      var done = false
      do{
	val (succ, mark) = victim.nextMark(level).get
	done = mark || 
	       victim.nextMark(level).compareAndSet((succ,false), (succ,true))
      } while(!done)
    } // end of for loop
    // Now try to mark at level 0
    while(true){
      val (succ, mark) = victim.nextMark(0).get
      if(mark) return false // already marked
      else if(victim.nextMark(0).compareAndSet((succ, false), (succ, true))){
	// above CAS is linearization point if successful
	find(x, key); return true
	// call to find is optimisation to remove links to victim
      }
      // else try again
    }
    sys.error("unreachable")
  }
}
