/** Some profiling functions */
package ox.cads.util

import scala.collection.mutable.ArrayBuffer

/** The object implements a number of named timers and named counters.  Code 
  * can be timed, with the time taken added to that of the named timer.  
  * Similarly, the number of times particular points in the code are reached
  * can be counted, with the count recorded against a named counter. 
  *
  * The normal usages are:
  *
  *  - `Profiler.time("timer-name"){ code }`,
  *    which times `code`, recording the time against timer `timer-name`, and
  *   returns the result of `code`;
  *  - `Profiler.count("counter-name")`, which adds one onto counter 
  *    `counter-name`;
  *  - `Profiler.report` which gives a summary of the profiling.
  *
  * Note that normal scoping rules apply.  In code such as
  * `Profiler.time("answer"){ val x = 6*7 }`, the scope of `x` will  be just
  * the inner statement; normal use would be 
  * `val x = Profiler.time("answer"){ 6*7 }`.
  *
  * For use with concurrent code, it is recommended to start by calling
  * `setWorkers(p)` to set the number of concurrent threads to `p` 
  * (8 by default).  If two threads have the same ID (mod `p`) then they
  * may interfere, both in terms of correctness, and through creating cache
  * contention.
  *
  * As with all profilers, there is a fairly significant overhead.
  */
object Profiler{
  /** Number of workers */
  private var p = 8

  /** The names of timers. */
  private var tnames = Array.fill(p)(new ArrayBuffer[String]())

  /** The values of timers.  times(w)(i) contains the time recorded by
    * worker w against timer tnames(w)(i). */
  private var times = Array.fill(p)(new ArrayBuffer[Long]())

  // Invariant: tnames.length = times.length = p
  // forall w in [0..p) . tnames(w).length = times(w).length
  // This represents the mapping 
  // { tname -> \sum { times(w)(i) | w <- [0..p), i <- [0..tnames(w).length), 
  //                                 tnames(w)(i) = tname } }

  /** The names of counters */
  private var cnames = Array.fill(p)(new ArrayBuffer[String]())

  /** The counters. */
  private var counts = Array.fill(p)(new ArrayBuffer[Int]())

  // Invariant: cnames.length = counts.length = p
  // forall w in [0..p) . cnames(w).length = counts(w).length
  // This represents the mapping 
  // { cname -> \sum { counts(w)(i) | w <- [0..p), i <- [0..cnames(w).length), 
  //                                  cnames(w)(i) = cname } }

  /** Set the number of concurrent workers */
  def setWorkers(p: Int) = { this.p = p; clear }

  /** Clear all counters and timers. */
  def clear = {
    tnames = Array.fill(p)(new ArrayBuffer[String]())
    times = Array.fill(p)(new ArrayBuffer[Long]())
    cnames = Array.fill(p)(new ArrayBuffer[String]())
    counts = Array.fill(p)(new ArrayBuffer[Int]())
  }

  /** Time cmd, recording the duration against the current thread */
  def time[A](tname:String)(cmd: => A) : A = {
    val w = ThreadID.get % p // this worker's ID
    val myNames = tnames(w); val myTimes = times(w)

    // Find the index for the timer tname
    var i = 0; 
    while(i<myNames.length && myNames(i)!=tname) i += 1
    if(i==myNames.length){ myNames += tname; myTimes += 0 }
    // start the timer
    val t0 = java.lang.System.currentTimeMillis()
    // run the code
    try{ cmd } finally {
      // stop the timer and record the duration
      val duration = java.lang.System.currentTimeMillis()-t0
      myTimes(i) += duration 
    }
  }

  // @deprecated("Two-argument notime", "21/10/2013") @inline 
  // def notime[A](w: Int, tname:String)(cmd: => A) : A = cmd

  /** Do not time cmd.
    * This function is provided to allow timing to be easily turned on and off.
    * Note, though, htat it has a non-zero cost. */
  @inline def notime[A](tname:String)(cmd: => A) : A = cmd

  /** Increment the counter, recording it against the current thread */
  def count(cname: String) = {
    val w = ThreadID.get % p // this worker's ID
    assert(w<p, "Not enough entries in Profiler for worker "+w+
	   "; use Profiler.setWorkers to set the number of workers.")
    val myNames = cnames(w); val myCounts = counts(w)
    // Find the index for the counter cname
    var i = 0
    while(i < myNames.length && myNames(i) != cname) i += 1
    if(i == myNames.length){ myNames += cname; myCounts += 0 }
    myCounts(i) += 1
  }

  /** Produce report of all timers and counters, and reset */
  def report = synchronized{
    // (timer name, time) pairs
    val tPairs0 = 
      (for(w <- 0 until p) yield tnames(w).zip(times(w))).flatten
    // (counter name, count) pairs
    val cPairs0 = 
      for(w <- 0 until p; i <- 0 until cnames(w).length)
      yield (cnames(w)(i), counts(w)(i))

    // Merge pairs with same name; pre: list is sorted. 
    def mergePairs[A : Numeric](pairs: Seq[(String, A)]) : Seq[(String, A)] = 
      if(pairs.isEmpty) pairs
      else{
	val (st,n) = pairs.head
	val (matches, others) = pairs.span(_._1==st)
	(st, matches.map(_._2).sum) +: mergePairs(others)
      }
    val tPairs : Seq[(String,Long)] = mergePairs(tPairs0.sorted)
    val cPairs : Seq[(String,Int)] = mergePairs(cPairs0.sorted)

    // max width of names
    val maxW = 
      if(tPairs.isEmpty && cPairs.isEmpty) -1
      else ( (for((tn,_) <- tPairs) yield tn.length) ++
             (for((cn,_) <- cPairs) yield cn.length) ) .max

    if(tPairs.nonEmpty){
      println("TIMERS:")
      for((tn,t) <- tPairs) println(tn+": "+(" "*(maxW-tn.length))+t)
      println
    }
    if(cPairs.nonEmpty){
      println("COUNTERS:")
      for((cn,c) <- cPairs) println(cn+": "+(" "*(maxW-cn.length))+c)
    }
    // Reset here?
  }

}
