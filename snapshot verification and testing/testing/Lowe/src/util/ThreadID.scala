
package ox.cads.util

/** An object providing thread identifiers.
  * Based on Herlihy & Shavit, Section A.2.4 */
object ThreadID{
  /** The next thread ID to use */
  private var nextID = 0
  /** Get the next thread ID to use */
  private def getNextID = synchronized{ nextID += 1; nextID-1 }

  /** This thread's ID, as a thread-local object */
  private object ThreadLocalID extends ThreadLocal[Int]{
    override def initialValue() : Int = getNextID 
  }

  /** Get this thread's ID */
  def get : Int = ThreadLocalID.get

  /** Set this thread's ID */
  def set(index: Int) = ThreadLocalID.set(index)

  /** Reset the next thread ID to use. 
    * Should be run only in a sequential setting. */
  def reset = nextID = 0
}
