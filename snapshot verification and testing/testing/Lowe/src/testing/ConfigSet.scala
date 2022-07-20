package ox.cads.testing

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Set

/** A trait representing a set of configurations. */
trait ConfigSet[S] extends Iterable[Configuration[S]]{
  /** Add c to the set */
  def +=(c: Configuration[S])

  /** Is this empty? */
  def isEmpty : Boolean

  /** size */
  def size : Int

  override def toString = map(_.toString).mkString("\n")
}

// ------------------------------------------------------------------

/** An implementation of ConfigSet based around an ArrayBuffer.
  * This tends to work better for smaller sets, since add is O(size). */
class ArrayBufferConfigSet[S] extends ConfigSet[S]{
  def this(c: Configuration[S]) = { this(); a += c }

  private val a = new ArrayBuffer[Configuration[S]]()

  def +=(c: Configuration[S]) = if(!a.contains(c)) a += c

  override def isEmpty = a.isEmpty

  // def forEach(op: Configuration[S] => Unit) = a.forEach(op)

  def iterator = a.iterator

  override def size = a.length
}

// ------------------------------------------------------------------

/** An implementation of ConfigSet based around a mutable set.  
  * This tends to work better for larger sets. */
class SetConfigSet[S <: AnyRef] extends ConfigSet[S]{
  private val s = Set[Configuration[S]]()

  def +=(c: Configuration[S]) = s += c

  override def isEmpty = s.isEmpty

  // def forEach(op: Configuration[S] => Unit) = a.forEach(op)

  def iterator = s.iterator

  override def size = s.size
}
