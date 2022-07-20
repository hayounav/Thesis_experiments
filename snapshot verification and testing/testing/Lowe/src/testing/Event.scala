package ox.cads.testing
import ox.cads.util.Profiler
/** Events used for logging operation calls and returns. */
class Event

/** Events used for logging invocations of operations.
  * @tparam S the type of the sequential specification object
  * @tparam A the type of the return value
  * @tparam B the type of the return of the operation on the sequential
  * object; this will be A for testers based on undoable sequential objects, 
  * and (A,S) for testers based on immutable objects. 
  * @param t the identity of the thread. 
  * @param msg a message describing the operation. 
  * @param op the corresponding operation on the sequential datatype. */
case class InvokeEvent[S,A,B](val t: Int, val msg: String, val op: S => B)  
extends Event{
  /** The corresponding ReturnEvent. */
  var ret: ReturnEvent[A] = null
  override def toString = t+" invokes "+msg
}

/** Event of a thread returning from an operation with result result.
  * @tparam A the type of the returned value.
  * @param t the identity of the thread. 
  * @param result the result returned. */
case class ReturnEvent[A](val t: Int, val result: A) extends Event{
  override def toString = t+" returns "+result
}

// -------------------------------------------------------

/** Trait for linked list nodes. */
trait LLNode{
  /** The next node in the linked list. */
  var next: LLNode = null

  /** The previous node in the linked list. */
  var prev: LLNode = null
} 

object LLNode{
  type LLEvent = Event with LLNode

  /** Build a linked list from the events in es, returning a dummy header. */
  def buildList(es: Array[LLEvent]) : LLEvent = {
    val logHeader = new Event with LLNode
    var logLast = logHeader; var index = 0
    for(e <- es){ 
      e match {
	case ie: LLInvokeEvent[_,_,_] => {}
	case re: LLReturnEvent[_] => { re.index = index; index += 1 }
      }
      e.prev = logLast; logLast.next = e; logLast = e 
    }
  logHeader
  }

  /** Remove inv and ret from the log */
  def lift(inv: LLEvent, ret: LLEvent){
    inv.prev.next = inv.next; inv.next.prev = inv.prev
    ret.prev.next = ret.next; if(ret.next != null) ret.next.prev = ret.prev
  }

  /** Replace inv and its return event into the log */
  def unlift(inv: LLInvokeEvent[_,_,_]){ 
    val ret = inv.ret.asInstanceOf[LLReturnEvent[_]]
    assert(ret.prev.next == ret.next); ret.prev.next = ret
    if(ret.next != null) ret.next.prev = ret
    assert(inv.prev.next == inv.next); inv.prev.next = inv; inv.next.prev = inv
  }

  /** Print the log, starting at logHeader. */
  def printLog(logHeader: LLNode) = {
    var n = logHeader.next
    while(n != null){ println(n); n = n.next }
    println
  }
}

// -------------------------------------------------------
// Events used in a linked list

/** Invocation events used in a linked list. */ 
class LLInvokeEvent[S,A,B](t1: Int, msg1: String, op1: S => B) 
extends InvokeEvent[S,A,B](t1, msg1, op1) with LLNode

/** Return events in a linked list. */
class LLReturnEvent[A](t1: Int, val result1: A) 
extends ReturnEvent[A](t1, result1) with LLNode{
  /** Sequence number of this relative to other ReturnEvents. */
  var index = 0 

  /** List of values that the sequential object allowed. */
  var results = new scala.collection.mutable.ArrayBuffer[A]()

  /** Compare this event's result with seqResult, returning true iff equal.
   * if equal, update results. */
  def compareResult(seqResult: A) : Boolean = {
    if(seqResult == result) true 
    else{
      if(!results.contains(seqResult)) results += seqResult
      false 
    }
  }
}

// -------------------------------------------------------
// Events used in the linearization algorithm for queue histories.

/** The superclass of all events in the linearization algorithm for queue 
  * histories.
  * @param t the identity of the thread. */
class QueueLinNode(val t: Int) extends Event{
  /** The next node in the linked list. */
  var next: QueueLinNode = null

  /** The previous node in the linked list. */
  var prev: QueueLinNode = null

  /** Next operation of this thread. */
  var nextLocal : QueueLinNode = null

  /** Previous operation of this thread. */
  var prevLocal : QueueLinNode = null

  /** Sequence number in the original history. */
  var origIndex = -1

  /** Sequence number. */
  var index = -1.0
  // Note the index is a double; this allows us to set the index appropriately
  // when we reorder events.

  /** Clone this. */
  override def clone = new QueueLinNode(t)

  /** Is this the invocation of a minimal unsuccessful dequeue operation? 
    * Here minimal means the operation starts before index eR.
    * @param eR the index of the earliest return. */
  def isUnsuccessfulDequeue(eR: Double) = false
}

/** Invocation events used in the queue linearization algorithm. */
//class QueueInvokeEvent(t: Int) extends QueueLinNode(t)

class EnqueueInvokeEvent[A](t: Int, val value: A) extends QueueLinNode(t){
  override def toString = t+" invokes enqueue of "+value
  override def clone = new EnqueueInvokeEvent[A](t, value)

  // var isLinearized = false

  /** A list of known enqueues of value that return during the call of this 
    * operation.  Initialised lazily. */
  // In fact, this proves slower than not caching this information. 
  // private var earlierSameValEnqs : List[EnqueueInvokeEvent[A]] = null

  /** Is this earliest ending? */
  // def isEarliestEnding : Boolean = Profiler.time("isEE"){
  //   // Initialise earlierSameValEnqs, if necessary
  //   if(earlierSameValEnqs == null){
  //     earlierSameValEnqs = List[EnqueueInvokeEvent[A]]()
  //     var ev = this.next
  //     while(ev != nextLocal){
  // 	if(ev.isInstanceOf[EnqueueReturnEvent]){
  // 	  val inv = ev.prevLocal.asInstanceOf[EnqueueInvokeEvent[A]]
  // 	  if(inv.value == value) earlierSameValEnqs ::= inv
  // 	}
  // 	ev = ev.next
  //     }
  //   }
  //   // Test if any member of earlierSameValEnqs is not linearized
  //   while(earlierSameValEnqs.nonEmpty && earlierSameValEnqs.head.isLinearized)
  //     earlierSameValEnqs = earlierSameValEnqs.tail
  //   earlierSameValEnqs.isEmpty
  // }

}

class DequeueInvokeEvent(t: Int) extends QueueLinNode(t){
  /** The result returned by this operation. */
  def result = nextLocal.asInstanceOf[DequeueReturnEvent[Any]].result

  override def isUnsuccessfulDequeue(eR:Double) = index < eR && result == None

  override def toString = t+ " invokes dequeue"
  override def clone = new DequeueInvokeEvent(t)
}

/** Return events used in the queue linearization algorithm. */
//class QueueReturnEvent(t: Int) extends  QueueLinNode(t)

class EnqueueReturnEvent(t: Int) extends QueueLinNode(t){
  override def toString = t+" returns enqueue"
  override def clone = new EnqueueReturnEvent(t)
}

class DequeueReturnEvent[A](t: Int, val result: Option[A]) 
extends QueueLinNode(t){
  override def toString = t+" returns dequeue: "+result
  override def clone = new DequeueReturnEvent[A](t, result)
}
