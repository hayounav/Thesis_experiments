package ox.cads.collection

import scala.reflect.ClassTag
import java.util.concurrent.atomic.AtomicInteger

/** A buffer, allowing capacity items to be added, and then for the 
  * items to be obtained as an array */
class BoundedBuffer[T: ClassTag](capacity: Int){
  private val elems = new Array[T](capacity)
  private val next = new AtomicInteger(0)

  /** Add x to the buffer.
    * @return the index where x is added. */
  def add(x: T) = {
    val index = next.getAndIncrement; elems(index) = x; index
  }

  /** Get an array containing all the values */
  def getAll = { 
    assert(next.get == capacity, 
	   "BoundedBuffer: filled "+next.get+" out of "+capacity)
    elems 
  }
}
