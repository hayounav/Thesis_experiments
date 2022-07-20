package ox.cads.collection

import java.util.concurrent.atomic.AtomicInteger

/** A priority queue using a binary tree.
  * Based on Herlihy & Shavit, Section 15.3. 
  * @param logRange the log of the range of priorities. */
class TreePriorityQueue[T >: Null](logRange: Int) extends PriorityQueue[T]{
  assert(logRange >= 1)

  // The range of priorities, and hence of leaves
  private val range = 1 << logRange
 
  // We build the tree out of the following type of nodes
  private abstract class TreeNode{
    var parent: InternalNode = null
  }

  // Leaves of the tree
  private class LeafNode extends TreeNode{
    // Data is held in the following; any type of pool will do
    private val queue = new LockFreeQueue[T]
    def put(item: T) = queue.enqueue(item)
    def get : T = queue.dequeue.get
  }

  // Internal leaves of the tree
  private class InternalNode
  extends TreeNode{
    var left, right: TreeNode = null
    // counter stores the number of values known to be in left
    private val counter = new AtomicInteger
    // Get the value of counter, and decrement but not below zero
    def boundedGetAndDecrement : Int = {
      var value = counter.get
      while(value != 0 && !counter.compareAndSet(value, value-1))
        value = counter.get
      value
    }
    // increment the value of counter
    def increment = counter.getAndIncrement
  }

  // Create tree
  private val leaf = Array.fill(range)(new LeafNode)
  private val internal = Array.fill(range-1)(new InternalNode)
  for(i <- 0 until (range/2-1)){
    internal(i).left = internal(2*i+1); internal(2*i+1).parent = internal(i)
    internal(i).right = internal(2*i+2); internal(2*i+2).parent = internal(i)
  }
  for(i <- (range/2-1) until range-1){
    internal(i).left = leaf(2*i+2-range)
    leaf(2*i+2-range).parent = internal(i)
    internal(i).right = leaf(2*i+3-range)
    leaf(2*i+3-range).parent = internal(i)
  }
  private val root = internal(0)

  /** Add item to the queue, with priority pri. */
  def add(item: T, pri: Int) = {
    assert(0 <= pri && pri < range && item != null)
    // Store item in appropriate leaf
    val myLeaf = leaf(pri); myLeaf.put(item)
    // Traverse up tree, incrementing appropriate counters
    var node: TreeNode = myLeaf
    while(node != root){
      val parent = node.parent
      if(node == parent.left) parent.increment
      node = parent
    }
  }

  /** Remove and return an item with minimal priority. */
  def removeMin : T = {
    var node : TreeNode = root
    while(true){
      node match{
	case in: InternalNode => {
	  if(in.boundedGetAndDecrement > 0) node = in.left 
	  else node = in.right
	}
	case leaf: LeafNode => return leaf.get
      }
    }
    sys.error("unreachable")
  }
}
