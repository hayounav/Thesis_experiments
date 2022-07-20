package ox.cads.collection
/** A termination protocol for p threads.  
  * 
  * Each thread should occasionally call '''noTerm(me)''' to indicate that it 
  * is not able to terminate.  When a thread is able to terminate, it should
  * repeatedly call '''canTerm(me)''' until it receives a result of '''true'''
  * or it is no longer able to terminate (e.g. because it received work to do
  * and so became active).
  *
  * ''Note:'' a thread should call '''canTerm''' only if it has done 
  * nothing that might cause another thread to become active since the last
  * time it called '''noTerm'''.
  */

class Termination(p: Int){
  /* Abstractly, the protocol works by a token being passed round the ring 
   * at least twice.  If a thread has called noTerm since the last time it 
   * handled the token, then termination is not possible; a new round is started
   * with the current thread as the controller.  If the circuit does two 
   * complete circuits, then the system can terminate. */

  assert(p > 1)

  // The token is represented by the following variables

  /** The thread about to receive the token.  
    * Note that only this thread will read or write the other fields. */
  @volatile private var at: Int = 1

  /** How many circuits has the token made in this round */
  private var circuit = 0

  /** Who is the controller of the current round? */
  private var controller = 0

  /** Can we terminate?  Set by controller. */
  @volatile private var done = false

  /** Have we been forced to terminate? */
  @volatile private var forceDone = false

  /** Have we been forced to terminate? */
  def forcedDone = forceDone

  /** busy(i) represents whether thread(i) has called noTerm since the last
    * time it handled the token. */
  private val busy = Array.fill(p)(false)

  private def next(me: Int) = (me+1)%p

  /** Worker me signals that it can terminate. */
  def canTerm(me: Int) : Boolean = {
    if(done || forceDone) return true
    else if(at == me){
      if(busy(me)){ // Need to start new round
	busy(me) = false; circuit = 0; controller = me; at = next(me); false
      }
      else if(me == controller){
	if(circuit == 1){ // second circuit complete; can terminate
	  done = true; true
	}
	else{ // start second circuit
	  circuit = 1; at = next(me); false
	}
      } // end of if(me == controller)
      else{ at = next(me); false } // not controller
    } // end of if(at == me)
    else false // token not here
  }

  /** Worker me indicates that it cannot terminate. */
  def noTerm(me: Int) = { assert(!done); busy(me) = true } 

  /** Force all threads to terminate. */
  def signalDone = forceDone = true

  /** Reset, ready for re-use. */
  def reset = {
    controller = 0; at = 1; circuit = 0; done = false
    for(i <- 0 until p) busy(i) = false
  }


}
