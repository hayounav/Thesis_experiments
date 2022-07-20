package ox.cads.locks

/** A condition associated with a lock. */
trait Condition{
  /** Wait for another thread to perform a signal. 
    *
    * The thread releases the lock when waiting, and reacquires the lock 
    * before continuing. */
  def await : Unit

  /** Wait until test becomes true, performing the test when another thread 
    * performs a signal.   */
  def await(test: => Boolean) : Unit = while(!test) await

  /** Signal to a thread performing an await */
  def signal

  /** Signal to all threads performing an await */
  def signalAll
}
  
