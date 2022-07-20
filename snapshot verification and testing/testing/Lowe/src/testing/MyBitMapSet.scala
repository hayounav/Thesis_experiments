
package ox.cads.testing
import ox.cads.util.Profiler

/** An implementation of bit maps, with an efficient check for equality.
  * The set contains elements from [0..size). */
class MyBitMapSet(size: Int){
  private val arraySize = (size-1) / 64 + 1 // ceiling(size/64)
  private var a = new Array[Long](arraySize)

  @inline private def entryFor(i: Int) = i >> 6
  @inline private def maskFor(i: Int) : Long = 1L << (i & 63)
  @inline private def inRange(i: Int) = assert(0 <= i && i < size)
  // This represents { i | a(entryFor(i)) & maskFor(i) != 0 }

  /** Is i in the set? */
  def apply(i: Int) = { inRange(i); (a(entryFor(i)) & maskFor(i)) != 0 }

  /** Add i to this. */
  def +=(i: Int) = { inRange(i); a(entryFor(i)) |= maskFor(i) }

  /** Remove i from this. */
  def -=(i: Int) = { inRange(i); a(entryFor(i)) &= ~maskFor(i) }

  /** Create a clone of this. */
  override def clone = { 
    val res = new MyBitMapSet(size); res.a = a.clone; res 
  }

  /** A MyBitMapSet formed by adding i to this. */
  def +(i: Int) : MyBitMapSet = { 
    inRange(i); val res = new MyBitMapSet(size); 
    var j = 0
    while(j < arraySize){ res.a(j) = a(j); j += 1 }  // res.a = a.clone
    res.a(entryFor(i)) |= maskFor(i); res
  }

  override def equals(other: Any) = other match{
    case that: MyBitMapSet => {
      var ix = 0; 
      while(ix < arraySize && a(ix) == that.a(ix)) ix += 1
      ix == arraySize
    }
  }

  override def hashCode = {
    //scala.util.hashing.MurmurHash3.arrayHash(a) -- a bit slow
    var h = 0x3c074a61; var ix = 0
    while(ix < arraySize){ 
      h = h * 0xb592f7af + a(ix).toInt + (a(ix)>>32).toInt; ix += 1 
    }
    h
  }
}

object MyBitMapSet{
  private val mask = Array.tabulate(32)( 1 << _ )
}
