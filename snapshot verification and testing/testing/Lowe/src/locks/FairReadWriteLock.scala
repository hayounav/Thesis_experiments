package ox.cads.locks

/** A readers-writers lock, that aims to prevent starvation of writers. 
  *
  * Based on Herlihy & Shavit, Section 8.3.2.
  */
class FairReadWriteLock extends ReadWriteLock{
  private var readers = 0 // current # readers in the CS
  private var writer = false // is there a writer in the CS
  private val mx = 
    Lock.FairLock // for mutual exclusion on this; needs to be fair
  private val condition = mx.newCondition

  /** The read lock */
  object readLock extends Lock{
    def lock = mx.mutex{
      condition.await(!writer) // wait for writer to leave
      readers += 1 // record my entry
    }

    def unlock = mx.mutex{
      readers -= 1 // record my exit
      if(readers == 0) condition.signalAll // signal to waiting writer
    }

    def tryLock = mx.mutex{
      if(!writer){ readers += 1; true } else false
    }
  }

  /** The write lock */
  object writeLock extends Lock{
    def lock = mx.mutex{
      condition.await(!writer) // wait until no writer ahead of me
      writer = true // record that I'm trying; readers are blocked
      condition.await(readers == 0) // wait for readers to leave
    }

    def unlock = mx.mutex{
      writer = false // record my exit
      condition.signalAll // signal to waiting reader or writer
    }

    def tryLock = mx.mutex{
      if(readers == 0 && !writer){ writer = true; true } else false
    }
  }
}
