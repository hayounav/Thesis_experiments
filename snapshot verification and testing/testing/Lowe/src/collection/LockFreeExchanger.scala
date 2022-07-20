/** An object that allows two threads to exchange values.
  * Based on H&S, Section 11.4.1. */

package ox.cads.collection

import ox.cads.atomic.AtomicPair

class LockFreeExchanger[T]{
  // Values to indcate the state of the slot
  private val Empty = 0; private val Filled = 1; private val Busy = 2
  // Value placed in the slot when empty
  private val emptyVal = null.asInstanceOf[T]
  private val slot = new AtomicPair[T,Int](emptyVal,Empty)
  /* The first thread will fill the slot, setting the state to Filled; the 
   * second thread will swap with its value, setting the state to Busy;
   * the first thread will take the other's value, setting the state back to 
   * Empty. */

  /** Try to exchange myItem for another item.  Try for at most timeout ns, 
    * returning None if no item is obtained. */ 
  def exchange(myItem: T, timeout: Long) : Option[T] = {
    val timeBound = System.nanoTime + timeout // bound for trying
    while(true){
      if(System.nanoTime > timeBound) return None // timeout
      val (yourItem, state) = slot.get
      state match{
	case Empty => {
	  if(slot.compareAndSet((yourItem, Empty), (myItem, Filled))){
	    while(System.nanoTime <= timeBound){
	      val (yourItem2, state2) = slot.get
	      if(state2 == Busy){
		slot.set(emptyVal, Empty); return Some(yourItem2)
	      }
	    } 
	    // timeout; try emptying slot
	    if(slot.compareAndSet((myItem, Filled), (emptyVal, Empty)))
	      return None
	    else{ // partner arrived just in time
	      val (yourItem2, state2) = slot.get
	      assert(state2 == Busy)
	      slot.set(emptyVal, Empty); return Some(yourItem2)
	    }
	  } // end of if(slot.compareAndSet...); if CAS fails, re-try
	} // end of case Empty
	case Filled => {
	  if(slot.compareAndSet((yourItem, Filled), (myItem, Busy)))
	    return Some(yourItem)
	  // otherwise another thread took yourItem; retry.
	} // end of case Filled
	case Busy => { } // other threads using slot; re-try
      } // end of state match
    }
    sys.error("unreachable")
  }
}
