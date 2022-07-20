package ox.cads.testing

import ox.cads.util.Profiler
import scala.collection.mutable.ArrayBuffer

/** A tester for linearizabilty.  
  * 
  * This is the tester referred to as the breadth-first search just-in-time 
  * linearization tester'' in the paper 
  * [[http://www.cs.ox.ac.uk/people/gavin.lowe/LinearizabiltyTesting/ ''Testing for Linearizability'', Gavin Lowe]].  
  * See that paper for more explanation of the technique. 
  * 
  * @tparam S the type of states of the corresponding sequential datatype 
  * @tparam C the type of states of the concurrent datatype being tested
  * @param seqObj the sequential object that acts as the specification for the
  * concurrent datatype
  * @param concObj the concurrent object being tested
  * @param p the number of workers
  * @param worker a worker thread, whose parameters will be its identity and 
  * this
  * @param invocs the number of event invocations
  * @param maxSize the maximum number of configurations to allow
  * @param verbose should verbose output be given
  */
class BFSJITLinTester[S <: AnyRef](
  seqObj: S, p: Int, maxSize: Long = -1, verbose: Boolean = false)
extends GenericSolver[S, Event]{

  // def mkInvoke(t: Int, msg: String, seqOp: S => Any) = 
  //   new InvokeEvent(t, msg, seqOp)

  // def mkReturn(t: Int, result: Any) = new ReturnEvent(t, result)

  val mkInvoke = new InvokeEvent[S,Any,Any](_,_,_)
  val mkReturn = new ReturnEvent[Any](_,_)

  /** Test whether the history given by events is linearizable, returning 
    * one of the values defined in Solver. */
  def solve(events: Array[Event]) : Int = {
    // Test for linearizability
    val initConf = new Configuration[S](seqObj, ThreadStates[S](p))
    var configs : ConfigSet[S] = new ArrayBufferConfigSet[S]; 
    configs += initConf
    val log = new ArrayBuffer[String]

    for(i <- 0 until events.size){
      events(i) match{
	case im @ InvokeEvent(t, msg, op) => {
	  log += (t+" invokes "+msg)
	  if(verbose) println(t+" invokes "+msg)
	  val op1 = op.asInstanceOf[S => (Any,S)]
	  configs.foreach(_.invoke(t, msg, op1, im.ret.result))
	}
	case ReturnEvent(t, result) => {
	  log += (t+" returns: "+result)
	  if(verbose) println(t+" returns: "+result)
	  // Choose the implementation of the config set based on the current
	  // size
	  val configs1 =
	    if(configs.size > 5) new SetConfigSet[S] 
	    else new ArrayBufferConfigSet[S]
	  for(c <- configs) c.logReturnJITLin(t, result, configs1)

	  if(configs1.isEmpty){
	    println("Impossible return: "+result)
	    val fw = new java.io.FileWriter("error.txt")
	    fw.write(log.mkString("\n")+"\n\n"); fw.write(configs.toString)
	    fw.write("Impossible return: "+result); fw.close
	    println(log.mkString("\n")); println(configs)
	    return Solver.Failure
	  }
	  configs = configs1
	}
      } // end of match
      // for(_ <- 0 until configs.size) Profiler.count("Configurations")
      if(maxSize > 0 && configs.size > maxSize){
	println("Too many configurations: "+configs.size)
	return Solver.OutOfSteam
      }	
    } // end of while loop
    Solver.Success  
  }
}
