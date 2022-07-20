package ox.cads.util
import java.util.concurrent.atomic.AtomicIntegerArray

/** A counter to be shared by p threads.
  * The implementation assumes that the threads have distinct identities, mod p
  */
class Counter(p: Int){
  // The counter is implemented by p AtomicIntegers, one for each thread.
  // These are stored in an AtomicIntegerArray.

  // We could use padding to avoid false cache sharing, but this slows down
  // get.  It seems to make very little difference. 
  private final val padding =  1 // 16 // it's not clear what's best. 
  private val size = p*padding
  private val counters = new AtomicIntegerArray(size)
  // This represents the sum of the individual counters. 

  /** Increment the counter */
  def inc = { 
    val me = ThreadID.get % p; val idx = me*padding
    counters.set(idx, counters.get(idx) + 1)
  }

  /** Get the value of the counter */
  def get : Int = {
    var sum = 0; var idx = 0
    while(idx < size){ sum += counters.get(idx); idx += padding }
    sum
    // Exercise: show this is linearizable when interleaved with just the inc
    // operator
  }

  /** Add delta on to the counter */
  def add(delta: Int) = { 
    val me = ThreadID.get % p; val idx = me*padding
    counters.set(idx, counters.get(idx) + delta)
  } 


}
