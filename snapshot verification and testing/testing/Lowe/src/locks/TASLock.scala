
package ox.cads.locks

import java.util.concurrent.atomic.AtomicBoolean

/** A lock based upon the test-and-set operation 
  * Based on Herlihy & Shavit, Chapter 7. */
class TASLock extends Lock{
  /** The state of the lock: true represents locked */
  private val state = new AtomicBoolean(false)

  /** Acquire the Lock */ 
  def lock = while(state.getAndSet(true)){ }

  /** Release the Lock */
  def unlock = state.set(false)
  
  /** Make one attempt to acquire the lock
    * @return a Boolean indicating whether the attempt was successful */
  def tryLock : Boolean = ! state.getAndSet(true)
}
