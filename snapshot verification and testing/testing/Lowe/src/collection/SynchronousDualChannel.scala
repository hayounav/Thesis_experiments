
package ox.cads.collection

import java.util.concurrent.atomic.AtomicReference

/** A synchronous channel, using a dual data structure.
  * Based on Herlihy and Shavit, Section 10.7. */
class SynchronousDualChannel[T] extends PartialQueue[T]{
  // The types of nodes
  private type Type = Boolean; 
  private val Send = true; private val Receive = false

  // We will build a linked list using nodes of the following type
  private class Node(myItem: Option[T], val nType: Type){
    val item = new AtomicReference(myItem)
    val next = new AtomicReference[Node](null)
  }

  private val firstNode = new Node(None, Send)
  private val head = new AtomicReference(firstNode) // dummy header
  private val tail = new AtomicReference(firstNode) // last node in list

  /** Send value on the channel. */
  def send(value: T) = {
    val node = new Node(Some(value), Send)
    var done = false
    do{
      val myTail = tail.get; val myHead = head.get
      if(myHead == myTail || myTail.nType == Send){
	// try to add node to the list
	val next = myTail.next.get
	if(myTail == tail.get){ // optimization
	  if(next != null)  // help by advancing tail
	    tail.compareAndSet(myTail, next) // then re-try
	  else if(myTail.next.compareAndSet(null, node)){
	    // have enqueued node
	    tail.compareAndSet(myTail, node) // advance tail
	    // wait for the item to be taken
	    while(node.item.get != None){ }
	    // now remove node if it's still in the queue
	    val myHead1 = head.get
	    if(myHead1.next.get == node)
	      head.compareAndSet(myHead1, node)
	    done = true
	  } // CAS failed; re-try
	} // tail changed; re-try
      } // end of if(myHead == myTail || myTail.nType == Send)
      else{
	// Queue contains just Receive nodes; try to pair with first
	val next = myHead.next.get
	if(myTail == tail.get && myHead == head.get){
	  // try to store value in next
	  done = next.item.compareAndSet(None, Some(value))
	  head.compareAndSet(myHead, next) // try to advance head, regardless
	  // if the first CAS failed, re-try
	} // else re-try
      }
    } while(!done)
  }

  /** Receive a value on the channel. */
  def receive : T = {
    val node = new Node(None, Receive)
    var done = false; var oresult : Option[T] = None
    do{
      val myTail = tail.get; val myHead = head.get
      if(myHead == myTail || myTail.nType == Receive){
	// try to add node to the list
	val next = myTail.next.get
	if(myTail == tail.get){ // optimization
	  if(next != null)  // help by advancing tail
	    tail.compareAndSet(myTail, next) // then re-try
	  else if(myTail.next.compareAndSet(null, node)){
	    // have enqueued node
	    tail.compareAndSet(myTail, node) // advance tail
	    // wait for item to be filled
	    while(node.item.get == None){ }
	    oresult = node.item.get // result to return
	    // now remove node if it's still in the queue
	    val myHead1 = head.get
	    if(myHead1.next.get == node)
	      head.compareAndSet(myHead1, node)
	    done = true
	  } // CAS failed; re-try
	} // tail changed; re-try
      } // end of if(myHead == myTail || myTail.nType == Receive)
      else{
	// Queue contains just Send nodes; try to pair with first
	val next = myHead.next.get
	if(myTail == tail.get && myHead == head.get){
	  // try to get value from next
	  oresult = next.item.get // provisional result
	  if(oresult != None) done = next.item.compareAndSet(oresult, None)
	  // if result == null, someone beat us to it
	  head.compareAndSet(myHead, next) // try to advance head, regardless
	  // if the first CAS failed, re-try
	} // else re-try
      }
    } while(!done)
    oresult.get
  }

  // Turn this into a channel, mainly to allow the PartialQueueTest to be used
  // with it.
  def enqueue(value: T) = send(value)
  def dequeue = receive

}
