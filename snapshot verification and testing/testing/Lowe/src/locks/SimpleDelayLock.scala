package ox.cads.locks

import java.util.concurrent.atomic.AtomicBoolean

/** A lock based upon the test-and-set operation with fixed-time backoff.
  * Based on Herlihy & Shavit, Section 7.4. 
  * @param delay the time in nanoseconds to delay when conflict detected.
  */
class SimpleDelayLock extends Lock{ 

  /** The state of the lock: true represents locked */
  val state = new AtomicBoolean(false)

  /** Acquire the lock */
  def lock = {
    var done = false
    do{
      while(state.get()){ }
      if(! state.getAndSet(true)) done = true
      else{
	//ox.cads.util.NanoSpin(delay) // this works less well than sleep
	Thread.sleep(1) 
      }
    } while(!done)
  }

  /** Release the lock */
  def unlock = state.set(false)

  /** Make one attempt to acquire the lock
    * @return a Boolean indicating whether the attempt was successful */
  def tryLock : Boolean = !state.get && !state.getAndSet(true)
}
