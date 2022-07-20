
package ox.cads.util

/** A general utility for making systems of parallel components */
object ThreadUtil{
  /** Create a thread that performs comp */
  def mkThread(comp: => Unit) : Thread = 
    new Thread(new Runnable{ def run = comp })

  /** Create a system of processes `proc(i)` for `i <- [0..p)`; run that system,
    * terminating when all the processes terminate. */
  def runIndexedSystem(p: Int, proc: Int => Unit) = {
    val threads = Array.tabulate(p)(i => mkThread(proc(i)))
    threads.foreach(_.start)
    threads.foreach(_.join)
  }

  /** Create a system of processes `proc` for `i <- [0..p)`; run that system,
    * terminating when all the processes terminate. */
  def runSystem(p: Int, proc: => Unit) = {
    val threads = Array.fill(p)(mkThread(proc))
    threads.foreach(_.start)
    threads.foreach(_.join)
  }

  /** Create a system from `p` and `q`; run the system, terminating when both 
    * terminate */
  def runParallel(p: => Unit, q: => Unit) = {
    val threads = Array(mkThread(p), mkThread(q))
    threads.foreach(_.start)
    threads.foreach(_.join)
  }



}
