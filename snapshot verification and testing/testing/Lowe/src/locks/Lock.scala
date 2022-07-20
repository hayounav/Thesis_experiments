package ox.cads.locks
 
/** A trait for locks.
  * Based on Herlihy & Shavit, Chapter 7.
  */
trait Lock{
  /** Acquire the Lock. */ 
  def lock : Unit

  /** Release the Lock. */
  def unlock : Unit 

  /** Execute comp protected by the lock. */
  def mutex[A](comp: => A) : A = { lock; try comp finally unlock }

  /** Make one attempt to acquire the lock
    * @return a Boolean indicating whether the attempt was successful. */
  def tryLock : Boolean

  /** Make one attempt to acquire the lock; if successful, execute comp and
    * then release the lock; if unsuccessful execute elseComp. */
  def tryLockCase[A](comp: => A)(elseComp: => A) : A = {
    if(tryLock){ try comp finally unlock } else elseComp
  }

  /** Get a new Condition object associated with this. */
  def newCondition : Condition = new SimpleCondition(this)
} 


// Companion object

object Lock{
  /** A default implementation of a Lock. */
  def apply() : Lock = new SimpleDelayLock

  def FairLock(p: Int) = new ArrayQueueLock(p)

  def FairLock = new CLHLock
}
