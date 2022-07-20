package ox.cads.collection

import java.util.concurrent.atomic.AtomicReference

/** An array that grows dynamically.
  * @tparam A the type of data stored in the array.
  * @param capacity the initial capacity of the array.
  * @param init computation to initialise entries in the array. */
class GrowableArray[A : scala.reflect.ClassTag]
    (capacity: Int, init: => A = null.asInstanceOf[A]){
  /** Constructor with default initialisation. */
  def this(capacity: Int) = this(capacity, null.asInstanceOf[A])

  private val table = Array.fill[A](capacity)(init)
  private val overflow = new AtomicReference[GrowableArray[A]](null)
  // This represents the array corresponding to table concatenated with the
  // array represented by overflow.
  // abs(ga) = ga.table ++ abs(ga.overflow),  if ga != null
  // abs(null) = empty array

  /** Get the value in position i. */
  def apply(i: Int) : A = 
    if(i < capacity) table(i) else{ grow; overflow.get.apply(i - capacity) }

  /** Update the value in position i with value a. */
  def update(i: Int, a: A) : Unit = 
    if(i < capacity) table(i) = a 
    else{ grow; overflow.get.update(i - capacity, a) }

  /** Expand this by initialising overflow. */
  private def grow =
    if(overflow.get == null){
      val rest = new GrowableArray[A](2*capacity, init)
      overflow.compareAndSet(null, rest)
    }
}
