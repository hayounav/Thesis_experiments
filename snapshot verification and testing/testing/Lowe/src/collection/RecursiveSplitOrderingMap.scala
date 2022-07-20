
package ox.cads.collection
import ox.cads.atomic.AtomicRefAnyPair
import java.util.concurrent.atomic.AtomicInteger

/** A lock-free map using recursive split ordering.
  * Based on Herlihy & Shavit, Section 13.3 */
class RecursiveSplitOrderingMap[A, B](capacity: Int) 
extends HashMap[A, B]{
  // We build the main linked list out of nodes of the following forms.
  abstract private class Node(val key: Int, n: Node){
    val nextMark = new AtomicRefAnyPair[Node,Boolean](n, false)
    def next = nextMark.getFirst
    def marked = nextMark.getSecond
  }

  // A sentinel node, used for navigation
  private class Sentinel(override val key: Int, n: Node) extends Node(key, n)

  // A node containing data.
  private class DataNode(override val key: Int, val arg: A, var datum: B, n: Node) 
  extends Node(key, n)

  // Nodes are ordered by key fields.  Each Sentinel has LSB 0, and each
  // DataNode has LSB 1.  The list represents the mapping formed from all 
  // arg -> datum pairs, for each unmarked DataNode in the list. 

  private val Mask = 0x3FFFFFFF // 30 least significant bits
  private val HiMask = 0x40000000 // 31st bit set

  // Take a 31 bit number, and reverse its binary representation
  private def reverse(i: Int) = {
    assert(i > 0) 
    var loMask = 1; var hiMask = HiMask; var result = 0
    for(_ <- 0 until 31){
      if((i & loMask) != 0) result |= hiMask
      loMask = loMask << 1; hiMask = hiMask >>> 1
    }
    result
  }

  /** Regular key based on h */
  private def mkRegularKey(h: Int) = reverse((h & Mask) | HiMask)

  /** Sentinel key based on h */
  private def mkSentinelKey(h: Int) = reverse(h & Mask)

  // Each DataNode(key, arg, datum) in the list has 
  // key = mkRegularKey(hash(arg))

  // The buckets, indexing into the linked list
  private val buckets = new GrowableArray[Sentinel](capacity)
  buckets(0) = new Sentinel(0, null) // the head of the linked list

  // The maximum number of buckets we can currently use; initial value is
  // fairly arbitrary.
  private val bucketSize = new AtomicInteger(2)

  // The size of the map
  private val size = new AtomicInteger(0)

  // Threshold for resizing 
  private val Threshold = 4

  // Resize if size > bucketSize * Threshold
  private def maybeResizeBuckets = {
    val mySize = size.getAndIncrement; val myBucketSize = bucketSize.get
    if(mySize > myBucketSize * Threshold){
      // assert(2*myBucketSize <= capacity, 
      // 	     "insufficient capacity for "+2*myBucketSize)
      bucketSize.compareAndSet(myBucketSize, 2*myBucketSize)
    }
  }

  /** Search, starting at start, for a node that satisfies stop.
    * Also remove any marked nodes encountered. 
    * @return a pair of successive nodes (pred, curr) such that curr is the 
    * first node after start that satisfies stop. */
  private def find(start: Sentinel, stop: Node => Boolean) : (Node, Node) = {
    var pred : Node = start; var curr = pred.next; var succ : Node = null
    while(true){
      var done = false
      // remove consecutive marked nodes from curr onwards
      while(!done && curr != null){
        val (s, marked) = curr.nextMark.get; succ = s
        if(marked){
          val ok = pred.nextMark.compareAndSet((curr,false), (succ,false))
          if(!ok) return find(start, stop) // re-start
          else curr = succ
        }
        else done = true
      } 
      // have we found item?
      if(stop(curr)) return (pred, curr)
      else{ pred = curr; curr = succ } // advance
    }
    sys.error("unreachable")
  }

  /** Search for the place to insert a DataNode(key, arg, _), starting from
    * start.
    * Also remove any marked nodes encountered. 
    * @return a pair of successive nodes (pred, curr) such that
    * (1) pred.key <= key <= curr.key (or curr = null); (2) curr.key = key 
    * iff curr.arg = arg; (3) if arg is anywhere, it's in curr.
    */
  private def find(start: Sentinel, key: Int, arg: A) : (Node, Node) = {
    assert((key & 1) == 1)
    // do we stop at node n?
    def stop(n: Node) : Boolean = n match{
      case null => true
      case sn : Sentinel => sn.key > key
      case dn : DataNode => dn.key > key || dn.key == key && dn.arg == arg
    }
    find(start, stop)
  }

  /** Search for the place to insert a sentinel with key key. 
    * Also remove any marked nodes encountered. 
    * @return  a pair of successive nodes (pred, curr) such that
    * pred.key < key <= curr.key (or curr = null). */
  private def findSentinel(start: Sentinel, key: Int)  : (Node, Node) = {
    assert((key & 1) == 0)
    def stop(n: Node) : Boolean = n == null || n.key >= key
    find(start, stop)
  }

  /** Get the sentinel node corresponding to hash h, creating it if
    * necessary. */
  private def getBucket(h: Int) : Sentinel = {
    val index = h & (bucketSize.get - 1) // % bucketSize.get
    if(buckets(index) == null) initializeBucket(index)
    buckets(index)
  }

  /** Create a sentinel node with index <code>index</code>, inserting it 
    * into the list and buckets. */
  private def initializeBucket(index: Int) : Unit = {
    // Find the index of parent: index with the most-significant bit cleared.
    // Find largest power of 2, <= index
    var k = bucketSize.get
    do{ k = k >> 1 } while(k > index)
    val parent = index - k
    // make sure parent exists
    if(buckets(parent) == null) initializeBucket(parent) 
    // Now search from bucket(parent), and insert mySentinel
    val key = mkSentinelKey(index);
    while(true){
      val (pred, curr) = findSentinel(buckets(parent), key)
      if(curr != null && curr.key == key){ // sentinel already there
	buckets(index) = curr.asInstanceOf[Sentinel]; return
      }
      else{
	// insert between pred and curr
	val mySentinel = new Sentinel(key, curr); 
	// mySentinel.nextMark.set(curr, false)
	if(pred.nextMark.compareAndSet((curr, false), (mySentinel, false))){
	  buckets(index) = mySentinel; return
	} // if CAS fails, retry.
      }
    }
    sys.error("unreachable")
  }

  /** Add the maplet arg -> datum to the map */
  def update(arg: A, datum: B) : Unit = {
    val myHash = hash(arg); val key = mkRegularKey(myHash)
    val bucket = getBucket(myHash)
    // Try to update, starting from bucket
    var done = false
    do{
      val (pred, curr) = find(bucket, key, arg)
      if(curr != null && curr.key == key){
	assert(curr.asInstanceOf[DataNode].arg == arg)
	curr.asInstanceOf[DataNode].datum = datum; done = true
      }
      else{
	// try to insert node between pred and curr
	val node = new DataNode(key, arg, datum, curr)
	done = pred.nextMark.compareAndSet((curr, false), (node, false))
	if(done) maybeResizeBuckets // else retry
      } 
    } while(!done)
  }

  /** Get the value associated with arg, or default is arg is not in the
    * map */ 
  def getOrElse(arg: A, default: => B): B = {
    val myHash = hash(arg); val key = mkRegularKey(myHash)
    val bucket = getBucket(myHash)
    val (pred, curr) = find(bucket, key, arg)
    curr match{
      case dn : DataNode if dn.key == key && ! dn.marked => dn.datum
      case _ => default
    }
  }

  /** Delete the value associated with arg */
  def delete(arg: A): Unit = {
    val myHash = hash(arg); val key = mkRegularKey(myHash)
    val bucket = getBucket(myHash); var done = false
    do{
      val (pred, curr) = find(bucket, key, arg)
      curr match{
	case dn : DataNode if dn.key == key && !dn.marked => {
	  assert(dn.arg == arg)
	  val succ = curr.next
	  // Try to mark curr to logically delete it
	  done = curr.nextMark.compareAndSet((succ, false), (succ, true))
	  // Then physically remove curr
	  if(done){
	    pred.nextMark.compareAndSet((curr, false), (succ, false))
	    size.getAndDecrement
	  }
	}
	case _ => done = true
      }
    } while(!done)
  }

  def getOrElseUpdate(arg: A, op: => B): B = {
    val myHash = hash(arg); val key = mkRegularKey(myHash)
    var opResult : Option[B] = None // result of call of op
    val bucket = getBucket(myHash)
    while(true){
      val (pred, curr) = find(bucket, key, arg)
      curr match{
	case dn : DataNode if dn.key == key && ! dn.marked => return dn.datum
	case _ => {
	  // try to insert node between pred and curr
	  if(opResult == None) opResult = Some(op) // only run op once
	  val datum = opResult.get
	  val node = new DataNode(key, arg, datum, curr)
	  //node.nextMark.set(curr, false)
	  val done = pred.nextMark.compareAndSet((curr, false), (node, false))
	  if(done){ maybeResizeBuckets; return datum } // else retry
	}
      }
    }
    sys.error("unreachable")
  }



  def clear: Unit = ???
}
