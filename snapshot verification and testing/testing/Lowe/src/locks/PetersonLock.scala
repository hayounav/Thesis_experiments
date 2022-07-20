
package ox.cads.locks

import ox.cads.util.ThreadID
import java.util.concurrent.atomic.AtomicIntegerArray

/** A lock based on Peterson's Algorithm */
class PetersonLock extends Lock{
  private val flag = new AtomicIntegerArray(2)
  @volatile private var victim = -1

  def lock = {
    val me = ThreadID.get; val other = 1-me 
    assert(me==0 || me==1, 
	   "ThreadID needs to be 0 or 1.  Try calling ThreadID.reset first")
    flag.set(me, 1); victim = me
    while(flag.get(other) == 1 && victim == me){ } // spin
  }

  def unlock = { val me = ThreadID.get; flag.set(me, 0) }

  def tryLock : Boolean = ???

}
