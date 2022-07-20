
package ox.cads.testing

import scala.collection.mutable.ArrayBuffer
import ox.cads.util.Profiler
import scala.collection.mutable.Map

/** A wrapper around a state of type S, but allowing equivalence classes to 
  * be easily identified. */
class StateWrapper[S](private var state: S){
  // Objects of this class are combined in a disjoint-set forest [CLR, Section
  // 22.3].

  // isRoot indicates that this is the root of its equivalence class, and so
  // state is the definitive state for the equivalence class.
  private var isRoot = true

  // If !isRoot, parent refers to the parent of this in its forest.
  private var parent : StateWrapper[S] = null

  /** Get the root StateWrapper of this equivalence class, and perform path
    * compression. */
  private def getRoot : StateWrapper[S] = 
    if(isRoot) this else{ parent = parent.getRoot; parent }

  /** Merge this into the equivalence class of that.
    * Pre: Both should be roots. */
  private def setParent(that: StateWrapper[S]) = {
    assert(isRoot && that.isRoot)
    isRoot = false; parent = that; state = null.asInstanceOf[S] // allow GC
    that.knownDistinct ++= this.knownDistinct
    that.previouslyFired ++= this.previouslyFired
  }

  /** Get the state associated with this. */
  def get : S = getRoot.state

  /** StateWrappers that are known to be different from this */
  private val knownDistinct = new ArrayBuffer[StateWrapper[S]]

  /** Record that that is known to be distinct from this */
  private def addDistinct(that: StateWrapper[S]) = knownDistinct += that

  /** Is that known to be distinct from this?  
    * that should be the root of its equivalence class. */
  private def isKnownDistinct(that: StateWrapper[S]) = 
    knownDistinct.exists(sw => sw.getRoot eq that)
    
  /** Equality test, but updating equivalence class information. */
  override def equals(other: Any) = other match{
    case (sw:StateWrapper[S]) => {
      val root = getRoot; val otherRoot = sw.getRoot
      if(root eq otherRoot) true  // same equivalence class
      else if(root.isKnownDistinct(otherRoot)) false // known different
      else if(root.state == otherRoot.state){
	// merge equivalence classes, making otherRoot the root
	root.setParent(otherRoot); true
      }
      else{ 
	// record that these are known to be distinct
	root.addDistinct(otherRoot); otherRoot.addDistinct(root); false 
      }
    }
  }

  private var h = 0 // memoized hash
  override def hashCode = { if(h==0) h = get.hashCode; h }

  /** Map recording which operations have previously been fired on this state,
    * and the result and resulting StateWrapper */
  private val previouslyFired = Map[String,(Any, StateWrapper[S])]()

  /** Simulate execution of op, but memoizing result; re-use result of 
    * previous operation if possible.
    * @param desc a string uniquely identifying this operation */
  def doOp(desc: String, op: S => (Any, S)) : (Any, StateWrapper[S]) =
    getRoot.doOpRoot(desc, op)

  /** As doOp, but on a root node */
  private def doOpRoot(desc: String, op: S => (Any, S)) 
    : (Any, StateWrapper[S]) 
  = {
    if(previouslyFired.contains(desc)) previouslyFired(desc)
    else{
      val (res, newState) = op(state)
      val newWrapper = 
	if(newState == state) this else new StateWrapper(newState)
      previouslyFired += desc -> (res, newWrapper)
      (res, newWrapper)
    }
  }
}
