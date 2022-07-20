package ox.cads.util
import scala.collection.mutable.Stack

/** A counter based on a combining tree.
  * Based on TAoMP, Section 12.3.
  * @param width the number of threads that will use this.
  */
class CombiningTree(width: Int){
  private val nodes = new Array[Node](width-1)
  nodes(0) = new Node
  for(i <- 1 until width-1) nodes(i) = new Node(nodes((i-1)/2))
  private val leafs = Array.tabulate((width+1)/2)(i => nodes(width-2-i))

  /** Increment the counter, and return its previous value. */
  def getAndIncrement : Int = {
    val myLeaf = leafs(ThreadID.get/2)
    var node = myLeaf
    // precombining phase
    while(node.precombine) node = node.parent
    val stop = node
    // combining phase 
    node = myLeaf; var combined = 1; val stack = new Stack[Node]
    while(node != stop){
      combined = node.combine(combined); stack.push(node); node = node.parent
    }
    // operation phase
    val prior = stop.op(combined)
    // distribution phase
    while(stack.nonEmpty){ node = stack.pop; node.distribute(prior) }
    prior
  }

  // -------- The Node class --------

  private class Node(val parent: Node){
    // Node statuses
    private val Idle = 0; private val First = 1; private val Second = 2;
    private val Result = 3; private val Root = 4
    private var status = Idle // status of this node
    private var locked = false // is this locked
    private var firstValue, secondValue = -1 // values deposited by threads
    private var result = 0 // result of this op/state for root

    // Constructor for the root node
    def this() = { this(null); status = Root }

    // precombining; result indicates whether thread should continue
    def precombine : Boolean = synchronized{
      while(locked) wait()
      status match{
	case Idle => { status = First; true }
	case First => { status = Second; locked = true; false }
	case Root => false
      }
    }

    // combining
    def combine(combined: Int) : Int = synchronized{
      while(locked) wait
      locked = true
      firstValue = combined // deposit my value
      status match{
	case First => combined
	case Second => 
	  combined + secondValue // combine with other thread's value
      }
    }

    // operation
    def op(combined: Int) : Int = synchronized{
      status match{
	case Root => { val prior = result; result += combined; prior }
	case Second => {
	  secondValue = combined // deposit my value
	  locked = false; notify // wake up other thread
	  while(status != Result) wait // wait for result
	  locked = false; notify // wake up other thread
	  status = Idle; result
	}
      }
    }
 
    // distribution
    def distribute(prior: Int) = synchronized{
      status match{
	case First => { status = Idle; locked = false }
	case Second => { result = prior + firstValue; status = Result }
      }
      notify
    }
  }
}
