package ox.cads.util

/** A class implementing exponential backoff.
  * Based on Herlihy & Shavit, Chapter 7.
  * @param minDelay the minimum length of delay, in nanos
  * @param maxDelay the maximum length of delay, in nanos
  */ 
class Backoff(minDelay: Int, maxDelay: Int){
  def this() = this(1, 1 << 20)

  /** The maximum length of the next delay */
  private var limit = minDelay

  /** Perform exponential backoff */
  def apply() = {
    val delay = 1+scala.util.Random.nextInt(limit) // time to delay (nanos) 
    limit = (2*limit) min maxDelay
    Thread.sleep(delay)
    // Thread.sleep(delay/1000000, delay%1000000)
  }
  // Note: my experiments suggest that taking delay non-zero gives much better
  // performance; hence the "+1" in the definition of delay.  The two-argument
  // version of Thread.sleep just rounds to the nearest millisecond, so
  // there's not much point in using it.

  /** Reset the limit */
  def reset = limit = minDelay
}
