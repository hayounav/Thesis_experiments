
package ox.cads.locks

import ox.cads.util.ThreadID
import java.util.concurrent.atomic.{AtomicInteger,AtomicIntegerArray}

/** A lock using an array to store waiting threads in a queue.
  * Based on Herlihy & Shavit, Section 7.5.1.
  * @param capacity the number of workers who can use the lock. */ 
class ArrayQueueLock(capacity: Int) extends Lock{
  private val padding = 16 // # words per cache line
  private val size = padding*capacity // # entries in the flag array

  // flag(i) is set to Go to indicate that the thread waiting on it can
  // proceed.  We only use slots that are a multiple of padding, to avoid
  // false sharing.
  private val flags = new AtomicIntegerArray(size)
  private val Go = 1; private val Wait = 0
  flags.set(0, Go) // other flags initially equal Wait

  private val tail = new AtomicInteger(0) // the next free slot / padding

  // Array, indexed by ThreadIDs, where threads store the index of their flag
  // in flags.
  private val slotIndices = new Array[Int](capacity)

  def lock = {
    val slot = (tail.getAndIncrement * padding) % size
    slotIndices(ThreadID.get) = slot
    while(flags.get(slot) == Wait){ } // spin on flag(slot)
  }

  def unlock = {
    val slot = slotIndices(ThreadID.get)
    flags.set(slot, Wait) // anyone waiting here must wait
    flags.set((slot+padding)%size, Go) // next thread can progress
  }

  // I don't think tryLock can be implemented. 
  def tryLock : Boolean = ???
}
