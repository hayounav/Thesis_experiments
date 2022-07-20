package ox.cads.locks

import java.util.concurrent.atomic.AtomicInteger

/** A lock using an array to store waiting threads in a queue.
  * Based on Herlihy & Shavit, Section 7.5.1.
  * @param capacity the number of workers who can use the lock. */ 
class SimpleArrayQueueLock(capacity: Int) extends Lock{
  // ThreadLocal variable to record the slot for this thread
  private val mySlotIndex = 
    new ThreadLocal[Int]{ override def initialValue = 0 }

  private val size = capacity // # entries in the flag array

  // flag(i) is set to true to indicate that the thread waiting on it can
  // proceed.  We only use slots that are a multiple of padding, to avoid
  // false sharing.
  @volatile private var flag = new Array[Boolean](size) 
  flag(0) = true

  private val tail = new AtomicInteger(0) // the next free slot / padding

  def lock = {
    val slot = tail.getAndIncrement % size
    mySlotIndex.set(slot)
    while(!flag(slot)){ } // spin on flag(slot)
  }

  def unlock = {
    val slot = mySlotIndex.get
    flag(slot) = false // anyone waiting here must wait
    flag((slot+1)%size) = true // next thread can progress
    flag = flag // make sure the above write is propogated
  }

  // I don't think tryLock can be implemented. 
  def tryLock : Boolean = ???
}
