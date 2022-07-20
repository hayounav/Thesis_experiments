package ox.cads.util

/** An object that will just spin for a certain number of iterations. */
object Spin{
  /** Spin for n iterations.
    * To a first approximation, on a typical machine, a value of n = 800 
    * corresponds to one microsecond, and a value of n = 1,200,000 
    * corresponds to one millisecond. */
  def apply(n: Long) : Unit = {
    @volatile var count = 0 // volatile to prevent compiler optimisations!
    while(count < n) count += 1
  }

  /** Receive a Long n on the command line, and estimate how long apply(n) 
    * spins for. */
  def main(args: Array[String]) = {
    val n = args(0).toLong
    val reps = 50 max (500000000L / n).toInt // number of repetitions to use.
    // warm up
    var count = 0
    while(count < reps){ apply(n); count += 1 }
    // Now do the timing
    val t0 = java.lang.System.nanoTime
    count = 0
    while(count < reps){ apply(n); count += 1 }
    val t1 = java.lang.System.nanoTime
    val ans = (t1-t0) / reps
    println(ans+" nanoseconds")
    if(ans > 10000) println(ans/1000+" microseconds")
  }

}
