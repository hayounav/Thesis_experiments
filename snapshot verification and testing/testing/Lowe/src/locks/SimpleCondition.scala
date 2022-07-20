package ox.cads.locks

/** An implementation of conditions, based on `synchronized` blocks. */

class SimpleCondition(lock: Lock) extends Condition{
  /** Wait for another thread to perform a signal. */
  def await : Unit = { synchronized{ lock.unlock; wait }; lock.lock }
  // Need to release monitor before the lock.lock in case a parallel thread is
  // doing lock.mutex{ cond.signal }, which could lead to deadlock.  Need to
  // obtain monitor before releasing the lock, in case another thread gets the
  // lock and signals before we start the wait.  

  /** Signal to another thread.  This thread must hold lock. */
  def signal = synchronized{ notify }

  /** Signal to all other threads.  This thread must hold lock. */
  def signalAll = synchronized{ notifyAll }
  // The signaller must obtain the lock before signalling to avoid the signal
  // being lost if the other thread hasn't yet released the mutex on this. 
}
  
