import collection.mutable.{ArraySeq, ArrayBuffer, HashMap}
import ox.cads.testing
import io.Source

/*
 * This is a wrapper for Gavin Lowe's linearizability testing tool: https://www.cs.ox.ac.uk/people/gavin.lowe/LinearizabiltyTesting/
 * This wrapper is purpose-written for running the Lowe's Competition solver for a single history.
 * This wrapper is purpose-written for testing Simple Snapshot histories.
 */
object SnapTest {
  // Instantiate the types for the two solvers taking part in the competition
  type A = Int
  type S = ImmutableSnapshot[A]
  type US = UndoableSnapshot[A]
  type INVOKE1 = testing.InvokeEvent[S, Any, Any]
  type INVOKE2 = testing.InvokeEvent[US, Any, Any]

  // Operations for the sequential snapshots
  def seqUpdate(i: Int, v: A)(s: S) : (Unit, S) = ((), s.update(i, v))
  def seqScan(i: Int)(s: S) : (ArraySeq[A], S) =  (s.scan(i), s)

  def main(args: Array[String]) : Unit = {
    // Expect two arguments: (0) a filename containing the history and (1) the number of processes participating in the history
    val filename = args(0)
    val p = args(1).toInt

    val input = Source.fromFile(filename).getLines

    val runtime = Runtime.getRuntime
    // val m0 = runtime.totalMemory
    
    val t0 = System.nanoTime()

    // Initalize the solver competition
    val solver1 = new testing.DFSGraphJITLinTester(new ImmutableSnapshot(p, 0), p)
    val solver2 = new testing.JITLinUndoTester(new UndoableSnapshot(p, 0), p)
    val comp_solver = new testing.CompetitionSolver(solver1, solver2)

    // translate the string encoding the history into an event array, which is the input Lowe's tool expects
    val openActions = new Array[(INVOKE1, INVOKE2)](p)
    val events = new ArrayBuffer[(testing.Event, testing.Event)]
    for (op <- input.next.split(';')) {
      val parts = op.split('!')
      val action = parts(0)
      val args = parts(1).split(',').map(_.toInt)

      action match {
        case "startUpdate" => {
          val a1 = solver1.mkInvoke(args(0), op, seqUpdate(args(0), args(1)))
          val a2 = solver2.mkInvoke(args(0), op, _.update(args(0), args(1)))
          events += ((a1, a2))
          openActions(args(0)) = ((a1, a2))
        }
        case "endUpdate" => {
          val openAction = openActions(args(0))
          val a1 = solver1.mkReturn(args(0), ())
          val a2 = solver2.mkReturn(args(0), ())
          openAction._1.ret = a1
          openAction._2.ret = a2
          events += ((a1, a2))
        }
        case "startScan" => {
          val a1 = solver1.mkInvoke(args(0), op, seqScan(args(0)))
          val a2 = solver2.mkInvoke(args(0), op, _.scan(args(0)))
          events += ((a1, a2))
          openActions(args(0)) = ((a1, a2))
        }
        case "endScan" => {
          val openAction = openActions(args(0))
          val z = (new ArraySeq[Int](p)).map(x => 0)
          for (i <- 1 until args.length){
            z(i-1) = args(i)
          }
          val a1 = solver1.mkReturn(args(0), z)
          val a2 = solver2.mkReturn(args(0), z)
          openAction._1.ret = a1
          openAction._2.ret = a1
          events += ((a1, a2))
        }
        case _ => throw new Exception("invalid action type: "+action)
      }
    }

    // System.out.println(events.length)

    val t01 = System.nanoTime()

    val t1 = System.nanoTime()
    // run Lowe's tool on the parsed history
    val res = comp_solver.solve(events.toArray)

    val t2 = System.nanoTime()
    // val m1 = runtime.totalMemory

    // Report the results and resource consumption statistics on stderr
    System.err.println("res: " + res  + "\tdata processing time: " + ((t01 - t0) / 1000000000.0) +
      "\tsolve time: " + ((t2 - t1) / 1000000000.0) + "\ttotal time: " + ((t2 - t0 - (t1 - t01)) / 1000000000.0) + "\tMemory used: " + (runtime.totalMemory / (1024.0 * 1024.0)))
  }
}



/*
 * Following are snapshot specification implementations used by Lowe's tool for testing history linearizability.
 */

// A: is the type of the data stored in the snapshot
// B: is the return type of the update method. Used only for technical purposes
//    (needed to support both the immutable snapshot as well as the undo-able one)
trait BaseSnapshot[A, B] {
    def update(index: Int, value: A) : B

    // The index is often unnecessary in the scan method
    // However, it is added to the signature to support 
    // non-annonymous algorithms (e.g., Bowman, 2011)
    def scan(index: Int) : ArraySeq[A]
}

// Simplifies the (standard) case in which the update method returns Unit
trait Snapshot[A] extends BaseSnapshot[A, Unit]

class ImmutableSnapshot[T](capacity: Int, zero: T) extends BaseSnapshot[T, ImmutableSnapshot[T]] {
    private var buffer =  new ArraySeq[T](capacity).map{_ => zero}

    def this(capacity: Int, zero: T, b : ArraySeq[T]){
        this(capacity, zero)
        this.buffer = b
    }
    
    def update (index: Int, value: T) : ImmutableSnapshot[T] = {
        val s = buffer.clone
        s(index) = value

        new ImmutableSnapshot[T](capacity, zero, s)
    }

    def scan(index: Int) : ArraySeq[T] = buffer.clone
}

class UndoableSnapshot[T](capacity: Int, zero: T) extends Snapshot[T] with scala.collection.mutable.Undoable{
  private var buffer =  new ArraySeq[T](capacity).map{_ => zero}

  // We keep objects of the following types in a stack to allow undoing. 
  private abstract class UndoEvent{ 
    def apply : Unit
  }
  private case class UndoUpdate(i: Int, v: T) extends UndoEvent{
    def apply = { buffer(i) = v }
  }
  private case object NullUndo extends UndoEvent{
    def apply = {}
  }

  private val undoStack = new scala.collection.mutable.Stack[UndoEvent]
   
  def update (index: Int, value: T) : Unit = {
    undoStack.push(UndoUpdate(index, buffer(index)))
    buffer(index) = value
  }

  def scan(index: Int) : ArraySeq[T] = {
    undoStack.push(NullUndo)
    buffer.clone
  }

  def undo = undoStack.pop.apply
}
