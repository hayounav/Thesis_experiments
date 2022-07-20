package ox.cads.testing

/** A log to be used by several threads.
  * @tparam S the type of the sequential specification object. 
  * @tparam C the type of the concurrent object. */

abstract class GenericLog[S, C]{

  /** Get a log for thread t to use. */
  def apply(t: Int) : GenericThreadLog[S, C]

  /** Get the contents of the log, as a single array of events. */
  def getLog : Array[Event]

}

// --------- Companion object ---------

object GenericLog{
  /** The type of functions to create invocation events.  
    * The parameters represent the identity of the thread, a string 
    * describing the operation, and the corresponding operation on the
    * sequential datatype. */
  type MkInvokeType[S] = (Int, String, S => Any) => InvokeEvent[S,Any,Any]

  /** The type of functions to create invocation events.  
    * The parameters represent the identity of the thread, and the 
    * result of the operation. */
  type MkReturnType = (Int, Any) => ReturnEvent[Any]
}
