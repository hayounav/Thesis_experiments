package ox.cads.atomic
import java.util.concurrent.atomic.AtomicReference

/** Each object of this class encapsulates an (A,B) pair, but allows various
  * atomic operations upon that pair.  Comparisons in CAS operations use
  * value equality (==).
  * The implementation is based on the implementation of 
  * java.util.concurrent.atomic.AtomicStampedReference. */
class AtomicPair[A,B](a: A, b: B){
  /** The main state, and atomic reference to the pair (a,b) */
  private val atomicRef = new AtomicReference[(A,B)](a,b) 
  // There is an important datatype invariant: the atomic reference changes
  // only when the value of the pair changes (as captured by compare).

  /** Get the value of the pair */
  def get : (A,B) = atomicRef.get

  /** Get the first element of the pair */
  def getFirst : A = atomicRef.get._1

  /** Get the second element of the pair */
  def getSecond : B = atomicRef.get._2

  /** Set the value of the pair */
  def set(a: A, b: B) = atomicRef.set((a,b))

  /** Set the value of the pair */
  def set(pair: (A,B)) = atomicRef.set(pair)

  /** Notion of equality used on the first (A) component in compareAndSet 
    * operations. 
    * Here the notion is value equality (==), but this is overwritten in 
    * subclasses. */
  @inline def compareA(a1: A, a2: A) : Boolean = a1 == a2

  /** Notion of equality used on the second (B) component in compareAndSet 
    * operations. 
    * Here the notion is value equality (==), but this is overwritten in 
    * subclasses. */
  @inline def compareB(b1: B, b2: B) : Boolean = b1 == b2

  /** Does pair equal (a, b), according to the notion of 
    * equality we're using in compareAndSet?  
    * The definition is based on compareA and compareB. */
  @inline def compare(pair: (A, B), a: A, b: B) : Boolean = 
    compareA(pair._1, a) && compareB(pair._2, b)

  /** If the two components are equal to expectedA and expectedB (according 
    * to compare) then update them to newA and newB and return true; otherwise 
    * return false. */
  def compareAndSet(expectedA: A, newA: A, expectedB: B, newB: B) : Boolean = {
    val current = atomicRef.get
    if(compare(current, expectedA, expectedB))
      compare((newA, newB), expectedA, expectedB) ||
      atomicRef.compareAndSet(current, (newA, newB))
      // if the above CAS fails, there was a time since the read of current
      // that the value of the pair was not equal to the expected values (by
      // the DTI mentioned above).
    else false
  }
  // Note: here both comparisons use value equality (==) rather than object 
  // equality (eq). 

  /** If the current value is equal to expected 
    * then update to new and return true; otherwise return false. */
  def compareAndSet(expected: (A,B), newv: (A,B)) : Boolean = 
    compareAndSet(expected._1, newv._1, expected._2, newv._2)

  /** If the first component equals expectedA (according to compareA) then 
    * update it to newA and return true; otherwise return false. */
  def compareAndSetFirst(expectedA: A, newA: A) : Boolean = {
    val current @ (oldA,oldB) = atomicRef.get
    if(compareA(oldA, expectedA)) 
      compareA(newA, expectedA) || 
      atomicRef.compareAndSet(current, (newA,oldB)) ||
      compareAndSetFirst(expectedA, newA)
      // If the CAS fails, it might have been that the first component was
      // equal to expectedA throughout, so we need to retry. 
    else false
  }
    
  /** If the second component equals expectedB (according to compareB) then 
    * update it to newB and return true; otherwise return false. */
  def compareAndSetSecond(expectedB: B, newB: B) : Boolean = {
    val current @ (oldA,oldB) = atomicRef.get
    if(compareB(oldB, expectedB)) 
      compareB(newB, expectedB) || 
      atomicRef.compareAndSet(current, (oldA,newB)) ||
      compareAndSetSecond(expectedB, newB)
      // If the CAS fails, it might have been that the second component was
      // equal to expectedB throughout, so we need to retry. 
    else false
  }
}

// -------------------------------------------------------

/** Each object of this class encapsulates an (A,B) pair, but allows various
  * atomic operations upon that pair.  The component is a reference object:
  * comparisons in CAS operations use reference equality (eq) on the A
  * component, but value equality (==) on the B component. */
class AtomicRefAnyPair[A <: AnyRef, B](a: A, b: B) extends AtomicPair(a, b){
  /** Notion of equality used on the first (A) component in compareAndSet 
    * operations.  Here the notion is reference equality (eq). */
  @inline override def compareA(a1: A, a2: A) = a1 eq a2
}

// -------------------------------------------------------

/** Each object of this class encapsulates an (A,B) pair, but allows various
  * atomic operations upon that pair.  Both components are reference objects:
  * comparisons in CAS operations use reference equality (eq) on both
  * components. */
class AtomicRefRefPair[A <: AnyRef, B <: AnyRef](a: A, b: B) 
extends AtomicPair(a, b){
  /** Notion of equality used on the first (A) component in compareAndSet 
    * operations.  Here the notion is reference equality (eq). */
  @inline override def compareA(a1: A, a2: A) = a1 eq a2

  /** Notion of equality used on the second (B) component in compareAndSet 
    * operations.  Here the notion is reference equality (eq). */
  @inline override def compareB(b1: B, b2: B) = b1 eq b2
}
