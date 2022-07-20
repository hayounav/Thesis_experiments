
package ox.cads.locks

import ox.cads.util.ThreadID
import java.util.concurrent.atomic.{AtomicIntegerArray, AtomicLongArray}

/** A lock based upon Lamport's Bakery Algorithm
  * Based on Herlihy & Shavit, Section 2.6.
  * @param n the number of threads that can use the lock. */
class BakeryLock(n: Int) extends Lock{
  private val flag = new AtomicIntegerArray(n)
  private val label = new AtomicLongArray(n)

  /** Is (label(x),x) < (label(y),y) under the lexicographic order? */
  private def smallerLabel(x: Int, y: Int) = {
    val lx = label.get(x); val ly = label.get(y)
    lx < ly || lx == ly && x < y
  }

  /** Get a label, guaranteed to be greater than any produced before the 
    * call to this function */
  private def getLabel(me: Int) = {
    var maxL = label.get(0)
    for(i <- 1 until n) maxL = maxL max label.get(i)
    label.set(me, maxL + 1)
  }

  /** Can thread me enter?  i.e. does it have a smaller label then all
    * other threads that are trying to enter? */
  private def canEnter(me: Int) : Boolean = {
    var k = 0
    while(k < n){
      if(flag.get(k) == 1 && smallerLabel(k, me)) return false
      else k += 1
    }
    true
  }

  def lock = { 
    val me = ThreadID.get; flag.set(me, 1); getLabel(me)
    // Wait while somebody with a smaller label than me is trying: 
    while(!canEnter(me)){ } 
  }

  def unlock = flag.set(ThreadID.get, 0)

  def tryLock : Boolean = ???
}
