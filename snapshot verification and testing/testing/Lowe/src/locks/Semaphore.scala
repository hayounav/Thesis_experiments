
package ox.cads.locks

/** A counting semaphore.
  * @param capacity the maximum value the semaphore may take. */
class Semaphore(capacity: Int){
  private var state = capacity // current value of the Semaphore
  private val lock = Lock() // for mutual exclusion
  private val condition = lock.newCondition // for signalling

  /** Acquire the semaphore. */
  def acquire = lock.mutex{
    condition.await(state > 0)
    state -= 1
  }

  /** Release the semaphore. */ 
  def release = lock.mutex{
    state += 1
    condition.signalAll
  }
}
