/** A lock-free queue that tries to perform memory reallocation in a buggy way.
  * Loosely Based on Herlihy & Shavit, Section 10.6.
  * @param p the number of workers. */
package ox.cads.collection

// import java.util.concurrent.atomic.AtomicReference
import ox.cads.util.ThreadID
import ox.cads.atomic.AtomicPair

class LockFreeQueueRecycle[T](p: Int) extends Queue[T]{
  // We build linked lists from Nodes of the following type
  class Node(var value: T){
    val nextStamp = new AtomicPair[Node,Int](null,0)
    def next = nextStamp.getFirst
    def stamp = nextStamp.getSecond
  }

  private val firstNode = new Node(null.asInstanceOf[T]) // initial header 
  private val head = new AtomicPair[Node,Int](firstNode,0)
  private val tail = new AtomicPair[Node,Int](firstNode,0)

  ThreadID.reset // this is a bit hacky!

  // freeLists(w) is a list of Nodes that worker w can draw on.
  private val freeList = Array.fill(p)(null: Node)

  /** Get a node, either from this worker's free list, or a new one */
  private def allocate(value: T) : Node = {
    val me = ThreadID.get
    if(freeList(me) == null) new Node(value)
    else{ 
      val n = freeList(me); freeList(me) = freeList(me).next
      val oldStamp = n.stamp; n.nextStamp.set((null,oldStamp+1))
      n.value = value; n
    }
  }

  /** Add a node to this worker's free list */
  private def free(n: Node) = {
    val me = ThreadID.get; val oldStamp = n.stamp
    n.nextStamp.set(freeList(me), oldStamp+1); freeList(me) = n
  }

  /** Add value to the queue */
  def enqueue(value: T) : Unit = {
    val node = allocate(value) // new Node(value); 
    var done = false
    while(!done){
      val (myTail, tStamp) = tail.get; 
      val (next, nStamp) = myTail.nextStamp.get
      if((myTail, tStamp) == tail.get)
	    // in case it's been changed (optimization)
	if(next == null){
	  if(myTail.nextStamp.compareAndSet((next, nStamp), (node, nStamp+1))){
	    tail.compareAndSet((myTail, tStamp), (node, tStamp+1))
	    done = true
	  } // else re-try
	} 
	else // next != null, try to advance tail
	  tail.compareAndSet((myTail, tStamp), (next, tStamp+1))  // and retry
      // else retry
    } //end of while loop
    // printList
  }

  /** Dequeue and return a value if the queue is non-empty; else return null */
  def dequeue : Option[T] = {
    var done = false; var result : Option[T] = None
    while(!done){
      val (myHead, hStamp) = head.get; val (myTail, tStamp) = tail.get
      val (next, nStamp) = myHead.nextStamp.get
      if((myHead, hStamp) == head.get)
	  // in case it's been changed (optimization)
	if(myHead == myTail){  // empty queue
	  if(next == null){ // empty queue, return null
	    result = None; done = true 
	  }
	  else // new item partially enqueued
	    tail.compareAndSet((myTail, tStamp), (next, tStamp+1)) 
	    // try to advance tail
	}
	else{ // non-empty queue
	  result = Some(next.value) // provisional result
	  if(head.compareAndSet((myHead, hStamp), (next, hStamp+1))){
	    free(myHead); done = true // free up old head
	  }
	  // else result already taken, re-try
	}
      // else retry
    } // end of while loop
    result
  }
}
