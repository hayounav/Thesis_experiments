package ox.cads.locks

/** A trait for reader-writer locks.  
  * Based on Herlihy & Shavit, Section 8.3.
  */
trait ReadWriteLock{
  /** A lock for read operations */
  val readLock : Lock

  /** A lock for write operations */
  val writeLock : Lock
}
