package ox.cads.collection

/** A synchronous channel. */
class SynchronousChannel[T] extends PartialQueue[T]{
  private var item : Option[T] = 
    None // the item being sent, or None if no value is available
  private val lock = ox.cads.locks.Lock()
  private val slotFull = lock.newCondition // for signalling item is set
  private val slotEmpty = lock.newCondition // for signalling item is taken
  private val senderWait = lock.newCondition // for signalling to next sender
  private var sending = false // is there a thread sending?

  /** Send value on the channel, synchronously. */
  def send(value: T) = lock.mutex{
    senderWait.await(!sending) // wait for previous sender to finish
    assert(item == None)
    sending = true     // my turn
    item = Some(value) // here's my value
    slotFull.signal    // signal to a receiver
    slotEmpty.await(item == None) // wait for receiver to take it
    sending = false    // I'm done
    senderWait.signal  // signal to next sender
  }

  /** Receive a value on the channel, synchronously. */
  def receive : T = lock.mutex{
    slotFull.await(item != None)             // wait for value
    val oresult = item; assert(oresult != None) // take it
    item = None                               // clear it
    slotEmpty.signal                          // signal to sender
    oresult.get                               // and we're done
  }

  // Turn this into a channel, mainly to allow the PartialQueueTest to be used
  // with it.

  def enqueue(value: T) = send(value)

  def dequeue = receive

}

