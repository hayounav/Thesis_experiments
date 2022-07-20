
package ox.cads.locks

import java.util.concurrent.atomic.AtomicBoolean

/** A lock based upon the test-and-set operation 
  * Based on Herlihy & Shavit, Chapter 7. */
class TTASLock extends Lock{
  /** The state of the lock: true represents locked */
  private val state = new AtomicBoolean(false)

  /** Acquire the lock */
  def lock = 
    do{
      while(state.get()){ } // spin until state = false
    } while(state.getAndSet(true)) // if state = true, retry

  // /** Acquire the lock */
  // def lock = {
  //   var done = false
  //   while(!done){
  //     while(state.get()){ }
  //     if(! state.getAndSet(true)) done = true
  //   }
  // }

  /** Release the lock */
  def unlock = state.set(false)

  /** Make one attempt to acquire the lock
    * @return a Boolean indicating whether the attempt was successful */
  def tryLock : Boolean = !state.get && !state.getAndSet(true)
}
