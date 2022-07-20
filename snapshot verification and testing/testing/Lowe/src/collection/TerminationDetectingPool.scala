package ox.cads.collection
import ox.cads.util.ThreadID

/** A wrapper around a partial Pool to give a total Pool.
  *
  * A call to get will block until either:
  * (1) A value can be returned; (2) All treads are calling get, at which
  * point all return None; or (3) A thread calls signalDone, at which point
  * each call to get returns None.
  * @param underlying the underlying Pool
  * @param p the number of threads. 
  */

class TerminationDetectingPool[T](underlying: Pool[T], p: Int) extends Pool[T]{
  private val term = new Termination(p)

  /** Add x to the underlying pool. */
  def add(x: T) = add(x, ThreadID.get%p) 

  /** Add x to the underlying pool.
    * @param me the identitiy of the thread doing the put. */
  def add(x: T, me: Int) = { 
    term.noTerm(me); underlying.add(x) 
  }

  /** Try to get from the pool, trying repeatedly until either a value is
    * available, or because termination is possible.
    * @return Some(x) where x is the value obtained, or None to indicate no
    * value is available. */
  def get : Option[T] = get(ThreadID.get%p)

  /** Try to get from the pool, spinning until either a value is available, 
    * or termination is possible.
    * @param me the thread doing the get.
    * @return Some(x) where x is the value obtained, or None to indicate no
    * value is available. */
  def get(me: Int) : Option[T] = {
    if(term.forcedDone) None
    else{
      var result = underlying.get
      while(result == None && !term.canTerm(me)) result = underlying.get 
      result
    }
  }

  /** Force all threads to terminate. */
  def signalDone = term.signalDone

  /** Reset the termination object. */
  def reset = term.reset 
}
