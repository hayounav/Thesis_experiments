package ox.cads.collection

import java.util.concurrent.atomic.AtomicReference

/** A lock-free stack.  Based on Herlihy & Shavit, Section 11.2 */
class LockFreeStack[T] extends TotalStack[T]{
  // We'll build linked lists from the following type of Nodes
  private class Node(val value: T){
    var next: Node = null
  }

  // Atomic reference to the top of the stack
  private val top = new AtomicReference[Node](null)
  
  // Back off; could use binary back off here
  private def pause = ox.cads.util.NanoSpin(500) 

  /** Push value onto the stack */
  def push(value: T) = {
    val node = new Node(value)
    var done = false
    do{
      val oldTop = top.get
      node.next = oldTop
      // try to add node to the stack
      done = top.compareAndSet(oldTop, node) 
      if(!done) pause // back off
    } while(!done)
  }

  /** Pop a value from the stack.  Return None if the stack is empty.  */ 
  def pop : Option[T] = {
    var result : Option[T] = None; var done = false
    do{
      val oldTop = top.get
      if(oldTop == null) done = true // empty stack; return None
      else{
	val newTop = oldTop.next
	// try to remove oldTop from list
	if(top.compareAndSet(oldTop, newTop)){
	  result = Some(oldTop.value); done = true
	}
	else pause
      }
    } while(!done)
    result
  }

}
