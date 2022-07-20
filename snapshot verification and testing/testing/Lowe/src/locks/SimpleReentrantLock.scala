package ox.cads.locks
import ox.cads.util.ThreadID

/** A re-entrant lock.
  * Based on Herlihy & Shavit, Section 8.4.
  */
class SimpleReentrantLock(private val mx: Lock = Lock()) extends Lock{
  private val condition = mx.newCondition
  private var owner = -1 // current owner of the lock
  private var holdCount = 0 // # times owner has locked

  def lock = {
    val me = ThreadID.get
    mx.mutex{
      if(owner == me) holdCount += 1 // I've locked again
      else{
	condition.await(holdCount == 0) // wait until nobody else in
	owner = me; holdCount = 1 // record that I'm in
      }
    }
  }

  def unlock = mx.mutex{
    assert(owner == ThreadID.get && holdCount != 0)
    holdCount -= 1 // record my exit
    if(holdCount == 0) condition.signal // wake up another thread
  }

  def tryLock = mx.mutex{
    val me = ThreadID.get
    mx.mutex{
      if(owner == me){ holdCount += 1; true } // I've locked again
      else if(holdCount == 0){ // nobody else in
	owner = me; holdCount = 1; true // record that I'm in
      }
      else false
    }
  }    

  /** Get a new Conditoin object associated with this ReentrantLock. */
  override def newCondition : Condition = new Condition{
    /** Wait for another thread to perform a signal. */
    def await = {
      val me = ThreadID.get; var myHoldCount = -1
      synchronized{ 
	mx.mutex{ // unlock
	  myHoldCount = holdCount // remember # times locked
	  assert(owner == me && myHoldCount != 0)
	  holdCount = 0          // release lock
	  condition.signal       // wake up another thread
	}
	wait      // wait to be woken
      }
      mx.mutex{   // lock
	condition.await(holdCount == 0) // wait until nobody else in
	owner = me; holdCount = myHoldCount // restore holdCount
      }
    }

    /** Signal to a thread performing an await. */
    def signal = synchronized{ notify }

    /** Signal to all threads doing an await. */
    def signalAll = synchronized{ notifyAll }
  } // end of newCondition

}

