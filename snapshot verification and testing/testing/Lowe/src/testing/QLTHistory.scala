package ox.cads.testing
import ox.cads.util.Profiler

import scala.collection.mutable.ArrayBuffer

/** A history for the QueueLinTester. 
  * @tparam A the type of data stored in the queue.
  * @param es an array of events to use in the list.
  * @param p the number of threads. 
  */

class QLTHistory[A](es: Array[QueueLinNode], p: Int, verbose: Boolean = false){
  
  /** The head and tail sentinels of the linked list. */
  private var head, tail : QueueLinNode = null

  // First event and first dequeue event of each node (both are necessarily
  // invoke events).
  private val firstEvent, firstDequeue = new Array[QueueLinNode](p)

  /* Invariants:
   * I1: the events are arranged in a doubly-linked list, with head and tail 
   *     sentinels;
   * I2: the events are ordered by index fields.
   * I3: for each event e, e.nextLocal points to the next event by this 
   *     thread, and e.prevLocal points to the previous event by this thread;
   * I4: firstEvent(t) points to the first event of thread t;
   * I5: firstDequeue(t) points to the first dequeue events of thread t.
   * (In the last three cases, the fields will be the tail sentinel if there
   * is no such event.)
   */

  // Initialise the list
  buildList(es)

  /** Build the linked list from es. */
  private def buildList(es: Array[QueueLinNode]) = {
    // Create linked list
    head = new QueueLinNode(-1) // dummy header
    var logLast = head; var index = 0 // last node; current index
    val lastEvents = new Array[QueueLinNode](p) // last node of each thread
    for(e <- es){ 
      // insert e in list; set index (I1, I2)
      e.prev = logLast; logLast.next = e; logLast = e
      index += 1; e.index = index; e.origIndex = index
      // link previous event by this thread to e, and vice versa (I3)
      val t = e.t
      if(lastEvents(t) != null) lastEvents(t).nextLocal = e
      e.prevLocal = lastEvents(t)
      lastEvents(t) = e
      // Initialise firstEvents(t), firstDequeue(t) (I4, I5)
      if(firstEvent(t) == null) firstEvent(t) = e
      if(e.isInstanceOf[DequeueInvokeEvent] && firstDequeue(t) == null)
	firstDequeue(t) = e
    }
    // Sentinel tail node
    tail = new QueueLinNode(-1); tail.index = index+1
    tail.prev = logLast; logLast.next = tail; tail.nextLocal = tail
    // Make sure I4 and I5 established in corner case; establish I3 for last
    // event of each thread.
    for(t <- 0 until p){
      if(firstEvent(t) == null) firstEvent(t) = tail
      if(firstDequeue(t) == null) firstDequeue(t) = tail
      if(lastEvents(t) != null) lastEvents(t).nextLocal = tail
    }
  }
  // Note the tail sentinel links back to itself, and has maximal index.

  /** Create clones of the non-sentinel nodes in the list, so it can be 
    * recreated. */
  protected[testing] def cloneList : Seq[QueueLinNode] = {
    var e = head.next
    val list = new scala.collection.mutable.ArrayBuffer[QueueLinNode]
    while(e != tail){
      list += e.clone; e = e.next
    }
    list
  }

  /** Convert the linked list to a string. */
  protected[testing] def listToString : String = {
    var e = head.next; var ix = head.index
    var result = ""
    while(e != tail){ 
      assert(e.index > ix); ix = e.index
      result += (e+"\n"); e = e.next 
    }
    assert(e.index > ix)
    result
  }

  // ----- Removing events, operations, etc -----

  /** Remove event e from the list, reestablishing I1. */
  private def removeEvent(e: QueueLinNode) = {
    e.prev.next = e.next; e.next.prev = e.prev
  }
  
  /** Remove the pair of events inv and ret, reestablishing I1 and I3.
    * Pre: these are matching invocation and return events. */
  private def removeOp(inv: QueueLinNode, ret: QueueLinNode) = {
    removeEvent(inv); removeEvent(ret)
    if(inv.prevLocal != null) inv.prevLocal.nextLocal = ret.nextLocal
    if(ret.nextLocal != null) ret.nextLocal.prevLocal = inv.prevLocal
  }

  /** Remove the pair of events inv and ret corresponding to a dequeue,
    * reestablishing I1, I3, I4 and I5.
    * Pre: these are matching invocation and return events. */
  protected[testing] def removeDequeue(inv: QueueLinNode) = {
    val ret = inv.nextLocal; val t = inv.t; removeOp(inv, ret)
    // Reestablish I4, I5
    firstEvent(t) = ret.nextLocal // first event by t
    firstDequeue(t) = nextDequeue(firstEvent(t)) // first dequeue by t
  }

  /** Remove minimal matching pair, e, d. */
  protected[testing] def removeMatchingPair(
      e: EnqueueInvokeEvent[A], d: DequeueInvokeEvent)
  = {
    val eRet = e.nextLocal
    val dRet = d.nextLocal.asInstanceOf[DequeueReturnEvent[A]]
    if(verbose)
      println("Removing matching pair: enqueue of "+e.value+" by "+e.t+
	      " and dequeue of "+dRet.result+" by "+d.t)
    // Remove e
    removeOp(e, eRet) // ; e.isLinearized = true
    firstEvent(e.t) = eRet.nextLocal // Reestablish I4

    // Find last return before d
    var prevRet = d.prev
    while(prevRet.isInstanceOf[EnqueueInvokeEvent[A]] || 
	  prevRet.isInstanceOf[DequeueInvokeEvent])
      prevRet = prevRet.prev

    // Remove d, and adjust events
    removeOp(d, dRet) // Remove d
    if(firstEvent(d.t) == d) // Reestablish I4 for d.t 
      firstEvent(d.t) = dRet.nextLocal
    firstDequeue(d.t) = nextDequeue(dRet.nextLocal)// Reestablish I5

    // For each concurrent dequeue d1 that starts before prevRet, move call of
    // d1 to between prevRet and prevRetNext, in order of indexes
    val prevRetNext = prevRet.next
    var count = 0 // # moved
    for(t1 <- 0 until p; if t1 != d.t){
      val d1 = firstDequeue(t1)
      if(d1.index < prevRet.index){ // need to move d1
	removeEvent(d1); count += 1 // remove d1
	// Insert d1 between prevRet and prevRetNext, in order of index
	var n = prevRet.next
	while(n != prevRetNext && n.index < d1.index) n = n.next
	// Insert d1 before n
	d1.next = n; d1.prev = n.prev; n.prev.next = d1; n.prev = d1
      }
    }
    // Now reset indexes
    val deltaIx = (prevRetNext.index-prevRet.index) / (count+1)
    var ix = prevRet.index+deltaIx; var n = prevRet.next 
    while(n != prevRetNext){
      n.index = ix; ix += deltaIx; n = n.next
    }
    assert(Math.abs(ix-prevRetNext.index) < 0.000001, 
	   ix+"; "+prevRetNext.index)
  }

  /** Remove matching pair, performed by et and dt. */
  protected[testing] def removeMatchingPairFor(et: Int, dt: Int) = {
    val e = firstEvent(et).asInstanceOf[EnqueueInvokeEvent[A]]
    val d = firstDequeue(dt).asInstanceOf[DequeueInvokeEvent]
    assert(e.value == d.result.get, e+"\n"+d)
    removeMatchingPair(e, d) // and continue
  }

  // ----- Simple searches and queries on the history -----

  /** The sequence number for the earliest return of an initial operation.
    * All operations whose call has an earlier sequence number are minimal. */
  private def earliestReturn : Double = {
    var t = 1; var result = firstEvent(0).nextLocal.index
    while(t < p){
      result = result min (firstEvent(t).nextLocal.index)
      t += 1
    }
    result
  }  

  /** Get a minimal successul dequeue, or null if there is no such. */
  protected[testing] def minUnsuccDeq : QueueLinNode = {
    var t = 0; val eR = earliestReturn
    while(t < p && ! firstEvent(t).isUnsuccessfulDequeue(eR)) t += 1
    if(t < p) firstEvent(t) else null
  }

  /** Are there no dequeues? */
  protected[testing] def noDequeues : Boolean = {
    var t = 0
    while(t < p && firstDequeue(t) == tail) t += 1
    t == p
  }

  /** The sequence number for the earliest return of a dequeue operation.
    * All dequeues whose call has an earlier sequence number are deq-minimal. 
    */
  private def earliestDequeueReturn : Double = {
    var t = 1; var result = firstDequeue(0).nextLocal.index
    while(t < p){
      result = result min (firstDequeue(t).nextLocal.index)
      t += 1
    }
    result
  }

  /** Get the original sequence number of the earliest remaining dequeue
    * return event, and all values that could be returned by that dequeue. */
  protected[testing] def getDebugInfo : (Int, ArrayBuffer[Option[A]]) = {
    // Traverse list to first dequeue return event; accumulate all values
    // enqueued before first return event
    var ev = head.next; var returns = new ArrayBuffer[Option[A]]() 
    var minimal = true // has there been no return event yet?
    while(! ev.isInstanceOf[DequeueReturnEvent[A]]){
      if(ev.isInstanceOf[EnqueueInvokeEvent[A]] && minimal)
	returns += Some(ev.asInstanceOf[EnqueueInvokeEvent[A]].value)
      else if(ev.isInstanceOf[EnqueueReturnEvent])
	minimal = false
      // If DequeueInvokeEvent, keep going
      ev = ev.next
    }
    if(minimal) returns += None
    (ev.origIndex, returns)
  }

  /** Find the next dequeue invocation by this thread, starting from the given 
    * invocation event, or tail if there is no such. */
  private def nextDequeue(e: QueueLinNode) : QueueLinNode = {
    var e1 = e
    while(e1 != tail && ! e1.isInstanceOf[DequeueInvokeEvent])
      e1 = e1.nextLocal.nextLocal
    e1
  }

  // -------- Finding minimal matching enqueues and dequeues

  /** Information about minimal enqueues or deq-minimal dequeues.  
    * A thread-ID-indexed array, for each giving the value enqueued/dequeued 
    * and index of the return event, or notFound if none is found. */
  private type MinInfo = Array[(A, Double)]

  private val nullA = null.asInstanceOf[A]
  private val notFound = (nullA, -1.0)

  // ---- Earlist ending matching pairs

  /** Find the minimal enqueues that end before any other 
    * enqueue of the same value; for each such, return the enqueued value
    * and index of the return event.
    * @return a thread-identity-indexed array, for each giving
    * the value enqueued and index of the return event, or notFound
    * if none is found. */
  private def getEarliestEndingEnqueues : MinInfo = {
    // Iterate through firstEvent to see which are minimal; for each,
    // store enqueued value and index of return in minEnqueues.
    val minEnqueues = Array.fill[(A,Double)](p)(notFound)
    val eR = earliestReturn
    for(t <- 0 until p){
      val e = firstEvent(t)
      if(e.isInstanceOf[EnqueueInvokeEvent[A]] && e.index < eR){
	// This is a minimal enqueue. 
	val inv = e.asInstanceOf[EnqueueInvokeEvent[A]]
	val value = inv.value 
	// val retIndex = e.nextLocal.index // index of return
	// Search for an earlier-ending enqueue of the same value
	var ev = /* head */ e.next; var isEE = true
	while(ev != e.nextLocal && isEE){
	  if(ev.isInstanceOf[EnqueueReturnEvent] &&       
             ev.prevLocal.asInstanceOf[EnqueueInvokeEvent[A]].value == value)
            isEE = false
	  ev = ev.next
	} // end of while loop
	// val isEE = inv.isEarliestEnding // less efficient in practice
	if(isEE) minEnqueues(t) = (value, e.nextLocal.index)
      }
    } // end of for loop
    minEnqueues
  }


  /** Find the deq-minimal successful dequeue that end before any other 
    * dequeue of the same value; for each such, return the enqueued value
    * and index of the return event.
    * @return a thread-identity-indexed array, for each giving
    * the value dequeued and index of the return event, or a negative index 
    * if none is found. */
  private def getEarliestEndingDequeues : MinInfo = {
    // Iterate through firstDequeue to see which are deq-minimal and
    // successful; for each, store return value and index of return in
    // minDequeues.
    val minDequeues = Array.fill[(A,Double)](p)(notFound)
    val eDR = earliestDequeueReturn
    for(t <- 0 until p){
      val d = firstDequeue(t)
      if(d != tail && d.index < eDR){ // This is a deq-minimal dequeue
	val ret = d.nextLocal.asInstanceOf[DequeueReturnEvent[A]]
	if(ret.result != None){ // This is successful
	  val result : Option[A] = ret.result
	  // Search for an earlier-ending dequeue of the same value
	  var ev = d.next; var isEE = true
	  while(ev != ret && isEE){
            if(ev.isInstanceOf[DequeueReturnEvent[A]] &&          
               ev.asInstanceOf[DequeueReturnEvent[A]].result == result)
              isEE = false
            ev = ev.next
	  } // end of while loop
	  if(isEE) minDequeues(t) = (result.get, ret.index)
	} // end of if(ret.result != None)
      }
    } // end of for loop
    minDequeues
  }

  /** Test if there is a conflicting unsuccessful dequeue for d. */
  private def isConflictingUnsuccDeq(d: DequeueInvokeEvent) : Boolean = {
    // Find latest ending enqueue e1 before d
    var ev = d.prev
    while(ev != head && ! ev.isInstanceOf[EnqueueReturnEvent])
      ev = ev.prev
    // Now scan through initial dequeues to see whether a successful and an
    // unsuccessful dequeue start before ev
    var foundSucc, foundUnsucc = false // have we found such yet?
    var index = ev.index
    for(t <- 0 until p){
      val inv = firstDequeue(t)
      if(inv.index < index){ // inv != tail here
	val res : Option[A] = 
	  inv.nextLocal.asInstanceOf[DequeueReturnEvent[A]].result
	if(res == None) foundUnsucc = true else foundSucc = true
      }
    }
    foundSucc && foundUnsucc
  }

  /** Find the earliest-starting deq-minimal unsuccessful dequeue.
    * @return optionally the invoke event of that dequeue. */
  private def getFirstUnsuccessfulDequeue : Option[DequeueInvokeEvent] = {
    var ix = tail.index // earliest index of unsuccessful dequeue so far
    var od = None : Option[DequeueInvokeEvent] // the result to return
    for(t <- 0 until p){
      val d = firstDequeue(t)
      if(d.index < ix){
	val ret = d.nextLocal.asInstanceOf[DequeueReturnEvent[A]]
	if(ret.result == None){ // new earliest unsuccessful dequeue
	  ix = d.index; od = Some(d.asInstanceOf[DequeueInvokeEvent])
	}
      }
    }
    od
  }

  /** Try to find matching enqueue and dequeue. */
  private def findMatch(enqs: MinInfo, deqs: MinInfo): Option[MatchingPair] = {
    var result = None : Option[(EnqueueInvokeEvent[A], DequeueInvokeEvent)]
    val firstUnsuccDeq = getFirstUnsuccessfulDequeue
    var t = 0
    while(t < p && result == None){
      val (v, ix) = enqs(t)
      if(ix >= 0){
	// try to find matching min dequeue
	var t1 = 0
	while(t1 < p && result == None){
	  val (v1, ix1) = deqs(t1)
	  if(ix1 >= 0 && v == v1){ // a match!
	    val enq = firstEvent(t).asInstanceOf[EnqueueInvokeEvent[A]]
	    val deq = firstDequeue(t1).asInstanceOf[DequeueInvokeEvent]
	    // Check for conflicting unsucessful dequeue
	    if(!isConflictingUnsuccDeq(deq)) result = Some((enq, deq))
	  }
	  t1 += 1
	} // end of while
      } //end of if(ix >= 0)
      t += 1
    } // end of while
    // No match found
    result
  }

  /** Find an earliest-ending minimal matching pair, if one exists. */
  protected[testing] def earliestEndingMinMatchingPair : Option[MatchingPair]
  = {
    // For each earliest-ending minimal enqueue, the value enqueued and the
    // index of the return event; indexed by thread IDs. 
    val minEnqueues : MinInfo = getEarliestEndingEnqueues
    // For each earliest-ending minimal successful dequeue, the value dequeued
    // and the index of the return event; indexed by thread IDs.
    val minDequeues : MinInfo = getEarliestEndingDequeues
    // Now try to find matches 
    findMatch(minEnqueues, minDequeues)
  }

  // --------- All min matching pairs

  /** Find the minimal enqueues; for each such, return the enqueued value
    * and index of the return event.   
    * @return a thread-identity-indexed array, for each giving the value
    * enqueued and index of the return event, or notFound if none is found. */
  private def getMinEnqueues : MinInfo = {
    val minEnqueues = Array.fill[(A,Double)](p)(notFound)
    val eR = earliestReturn
    for(t <- 0 until p){
      val e = firstEvent(t)
      if(e.isInstanceOf[EnqueueInvokeEvent[A]] && e.index < eR){
	// This is a minimal enqueue.  
	val value = e.asInstanceOf[EnqueueInvokeEvent[A]].value 
	val retIndex = e.nextLocal.index // index of return
	minEnqueues(t) = (value, retIndex)
      }
    }
    minEnqueues
  }

  /** Remove non-earliest-ending entries from minInfo, replacing them with 
    * notFound. */
  // private def removeNonEarliestEnding(minInfo : MinInfo) : MinInfo = {
  //   for(t <- 0 until p){
  //     val (v, ix) = minInfo(t)
  //     if(ix >= 0){
  // 	// Try to find another enqueue of v
  // 	for(t1 <- 0 until t){
  // 	  val (v1, ix1) = minInfo(t1)
  // 	  if(v == v1 && ix1 >= 0){
  // 	    if(ix < ix1) minInfo(t1) = notFound // t's op is earlier ending
  // 	    else minInfo(t) = notFound // t1's op is earlier ending
  // 	  }
  // 	} // end of for(t1 <- ...)
  //     }
  //   } // end of for(t <- ...)
  //   minInfo
  // }

  /** Find the deq-minimal successful dequeues; for each such, return the 
    * enqueued value and index of the return event.
    * @return a thread-identity-indexed array, for each giving
    * the value dequeued and index of the return event, or a negative index 
    * if none is found. */
  private def getMinDequeues : MinInfo = {
    val minDequeues = Array.fill[(A,Double)](p)(notFound)
    val eDR = earliestDequeueReturn
    for(t <- 0 until p){
      val d = firstDequeue(t)
      if(d != tail && d.index < eDR){ // This is a deq-minimal dequeue
	val ret = d.nextLocal.asInstanceOf[DequeueReturnEvent[A]]
	if(ret.result != None){ // This is successful
	  minDequeues(t) = (ret.result.get, ret.index)
	}
      }
    }
    minDequeues
  }

  /** A matching pair of enqueue and dequeue events. */
  private type MatchingPair = (EnqueueInvokeEvent[A], DequeueInvokeEvent)

  // /** Find a minimal matching pair, if one exists. */
  // private def minMatchingPair : Option[MatchingPair] = {
  //   // For each minimal enqueue, the value enqueued and the index of the
  //   // return event; indexed by thread IDs.
  //   val minEnqueues : MinInfo = getMinEnqueues
  //   // For each minimal successful dequeue, the value dequeued and the index
  //   // of the return event; indexed by thread IDs.
  //   val minDequeues : MinInfo = getMinDequeues
  //   // Now try to find matches 
  //   findMatch(minEnqueues, minDequeues)
  // }

  /** Find all minimal matching pairs. */
  protected[testing] def allMinMatchingPairs : Seq[MatchingPair] = {
    // For each minimal enqueue, the value enqueued and the index of the
    // return event; indexed by thread IDs.
    val minEnqueues : MinInfo = getMinEnqueues
    // For each minimal successful dequeue, the value dequeued and the index
    // of the return event; indexed by thread IDs.
    val minDequeues : MinInfo = getMinDequeues
    // Now try to find matches 
    val result = new ArrayBuffer[MatchingPair]
    for(t <- 0 until p){
      val (v, ix) = minEnqueues(t)
      if(ix >= 0){
	// try to find matching min dequeue
	for(t1 <- 0 until p){
	  val (v1, ix1) = minDequeues(t1)
	  if(ix1 >= 0 && v == v1){ // a match!
	    val enq = firstEvent(t).asInstanceOf[EnqueueInvokeEvent[A]]
	    val deq = firstDequeue(t1).asInstanceOf[DequeueInvokeEvent]
	    result += ((enq, deq))
	  }
	}
      }
    }
    result
  }


}
