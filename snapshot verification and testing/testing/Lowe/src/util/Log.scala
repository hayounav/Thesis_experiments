package ox.cads.util

import java.io._

/** Classes to perform various types of logging.
  *
  * The recommended use of any code using a logger log is 
  * try{ ... log("whatever") ... }{finally log.shutdown }
  */

trait Log{
  /** Log message msg */
  def apply(msg: => String) : Unit
 
  /** Shut-down the log cleanly */
  def shutdown : Unit
}

/** A logging object that does nothing with its arguments */
final object NullLog extends Log{
  /** Log message msg */
  @inline def apply(msg: => String) : Unit = {}
 
  /** Shut-down the log cleanly */
  def shutdown : Unit = {}
}

/** A logging object that just prints to the screen */
final object ScreenLog extends Log{
  /** Log message msg */
  @inline def apply(msg: => String) : Unit = println(msg)
 
  /** Shut-down the log cleanly */
  def shutdown : Unit = {}
}

/** A logging object that writes to a file
  * @param fname the name of the file to use for the log */
final class FileLog(fname: String) extends Log{
  private val writer = new PrintWriter(fname)

  /** Log message msg */
  @inline def apply(msg: => String) : Unit = writer.write(msg+"\n")
 
  /** Shut-down the log cleanly */
  def shutdown : Unit = { writer.flush(); writer.close() }
}
