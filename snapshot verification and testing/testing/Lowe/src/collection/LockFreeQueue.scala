/** A lock-free queue.
  * Based on Herlihy & Shavit, Section 10.5 */
package ox.cads.collection

import java.util.concurrent.atomic.AtomicReference

class LockFreeQueue[T] extends Queue[T]{
  // We build linked lists from Nodes of the following type
  class Node(val value: T){
    val next = new AtomicReference[Node](null)
  }

  private val firstNode = new Node(null.asInstanceOf[T]) // initial dummy header 
  private val head = new AtomicReference(firstNode)
  private val tail = new AtomicReference(firstNode)

  /** Add value to the queue */
  def enqueue(value: T) : Unit = {
    val node = new Node(value); var done = false
    while(!done){
      val myTail = tail.get; val next = myTail.next.get
      if(myTail == tail.get) // in case it's been changed (optimization)
	if(next == null){
	  if(myTail.next.compareAndSet(next, node)){
	    tail.compareAndSet(myTail, node); done = true
	  } // else re-try
	} 
	else // next != null, try to advance tail
	  tail.compareAndSet(myTail, next) // and retry
      // else retry
    } //end of while loop
    // printList
  }

  /** Dequeue and return a value if the queue is non-empty; else return null */
  def dequeue : Option[T] = {
    var done = false; var result : Option[T] = None
    while(!done){
      val myHead = head.get; val myTail = tail.get; val next = myHead.next.get
      if(myHead == head.get) // in case it's been changed (optimization)
	if(myHead == myTail){  // empty queue
	  if(next == null){ // empty queue, return null
	    result = None; done = true 
	  }
	  else // new item partially enqueued
	    tail.compareAndSet(myTail, next) // try to advance tail
	}
	else{ // non-empty queue
	  result = Some(next.value) // provisional result
	  if(head.compareAndSet(myHead, next)) done = true 
	  // else result already taken, re-try
	}
      // else retry
    } // end of while loop
    result
  }

  /** Is the queue empty */
  def isEmpty : Boolean = head.get == tail.get
}
