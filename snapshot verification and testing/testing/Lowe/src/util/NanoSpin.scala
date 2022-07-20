package ox.cads.util

/** An object that will spin for a certain number of nanoseconds. */
import java.lang.System.nanoTime

object NanoSpin{
  /** Spin for about delay nanoseconds.
    * The accurracy is likely to be of the order of 100ns. */
  def apply(delay: Long) = {
    val end = nanoTime + delay
    while(nanoTime < end){ }
  }
}
