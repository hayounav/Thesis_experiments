package ox.cads.testing

import scala.reflect.ClassTag

/** An object with a timestamp. */
class TS[A](val body: A){
  /** A timestamp for this event */
  val ts = java.lang.System.nanoTime

  override def toString = "("+ts+", "+body+")"
}
	  
// --------- Companion object ---------

object TS{
  /** Merge several arrays of TS objects, returning the unstamped versions.
    * pre: each individual array is sorted by timestamps. */
  def merge[A: ClassTag](logs: Array[Array[TS[A]]]) : Array[A] = {
    val p = logs.length
    val size = logs.map(_.length).sum // total # elements
    val result = new Array[A](size)
    val indices = Array.fill(p)(0) 
    var next = 0 // next slot in result
    // Invariant: result[0..next) is the result of merging
    // { as(i)[0..indices(i)) | i <- [0..p) }.

    val sentinel = logs.map(_.last.ts).max + 1 // bigger than any entry
    // The timestamp of the next element of as(i) to consider, or sentinel
    def get(i: Int) : Long = {
      val ix = indices(i); if(ix < logs(i).length) logs(i)(ix).ts else sentinel
    }

    while(next < size){
      // find largest next el
      var min = get(0); var minIx = 0 // min timestamp found, and its index
      var i = 0
      while(i < p){
	val ts = get(i)
	if(ts < min){ min = ts; minIx = i }
	i += 1
      }
      assert(min < sentinel)
      result(next) = logs(minIx)(indices(minIx)).body
      next += 1; indices(minIx) += 1
    }

    // Check all finished
    for(i <- 0 until p) assert(indices(i) == logs(i).length)
    result
  }
}
