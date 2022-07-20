package ox.cads.locks

/** A readers-writers lock.
  * Based on Herlihy & Shavit, Section 8.3.1.
  */
class SimpleReadWriteLock extends ReadWriteLock{
  private var readers = 0 // current # readers in the CS
  private var writer = false // is there a writer in the CS
  private val mx = Lock() // for mutual exclusion on this
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
      condition.await(readers == 0 && !writer) // wait until empty
      writer = true // record my entry
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
