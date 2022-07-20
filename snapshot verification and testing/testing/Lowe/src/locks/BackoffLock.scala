

package ox.cads.locks

import java.util.concurrent.atomic.AtomicBoolean
import ox.cads.util.NanoSpin
import scala.util.Random

/** A lock based upon the test-and-set operation 
  * Based on Herlihy & Shavit, Section 7.4. 
  * @param minDelay the minimum length of delay, in nanos
  * @param maxDelay the maximum length of delay, in nanos
  */
class BackoffLock(minDelay: Int = 1, maxDelay: Int = 1<<20, 
		  random: Random = new Random) 
extends Lock{ 

  /** The state of the lock: true represents locked */
  val state = new AtomicBoolean(false)

  /** Acquire the lock */
  def lock = {
    var limit = minDelay; var done = false
    do{
      while(state.get()){ }
      if(! state.getAndSet(true)) done = true
      else{
	val delay = 1+random.nextInt(limit) // time to delay (millis) 
	limit = (2*limit) min maxDelay
	// NanoSpin(delay)
	Thread.sleep(delay)
      }
    } while(!done)
  }


  /** Release the lock */
  def unlock = state.set(false)

  /** Make one attempt to acquire the lock
    * @return a Boolean indicating whether the attempt was successful */
  def tryLock : Boolean = !state.get && !state.getAndSet(true)
}
