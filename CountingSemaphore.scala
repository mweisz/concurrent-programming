import io.threadcso._

trait Semaphore {
    def aquire: Unit
    def release: Unit
}

class CountingSemaphoreFQ(private var available: Int) extends Semaphore{
    val mutex = new BooleanSemaphore(true)
    val queue = new scala.collection.mutable.Queue[Flag]

    def aquire: Unit = {
        mutex.aquire
        if (available > 0) { // Is availability left
            // Take one of the availability 'tokens'
            available -= 1
            mutex.release
        } else { // All availabilities are used
            
            // Create flag to wait for and enqueue it            
            val flag = new Flag
            queue.enqueue(flag)

            mutex.release

            // Wait for the flag
            flag.aquire
        }

    }

    def release: Unit = {
        mutex.aquire


        if (queue.isEmpty) {
            available += 1
        } else {
            val flag = queue.dequeue()
            flag.release
        }

        mutex.release

    }
}