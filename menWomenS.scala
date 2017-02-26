import io.threadcso._
import io.threadcso.semaphore._

trait Matching {
    def manEnter: Unit
    def womanEnter: Unit
}

class MenWomenSemaphore extends Matching {
    // Use mutex to ensure atomicity of functions
    private val mutex = new BooleanSemaphore(available=true)


    private val womanIsAvailable = new BooleanSemaphore(available=false)

    override def manEnter() = {
        // mutex.acquire()

        womanIsAvailable.acquire()
        val v = womenQueue.dequeue()

        v.up()

        if (womenQueue.isEmpty) {
            womanIsAvailable.down()
        }

        // mutex.release()
        // Indicate that one woman is free to continue
    }

    override def womanEnter() = {
        mutex.acquire()
        val v = new BooleanSemaphore(available=false)

        // Indicate that a woman is available
        womenQueue.enqueue(v)
        womanIsAvailable.up()

        mutex.release()

        // The process will wait until a man has made v available
        v.acquire()
    
        // Now the woman is free to continue        
    }
}

object MenWomenSemaphoreTest extends MenWomenTest(new MenWomenSemaphore)



// Test Code
class MenWomenTest(matching: Matching) { 
    def main(args: Array[String]) = { 
    val N  = if (args.size>0&&args(0).matches("[0-9]+")) args(0).toInt else 200
    val start = nanoTime
    def stamp = nanoTime-start
    def Men = for (i<-0 until N) 
             fork(proc (s"Man$i"){ matching.manEnter; println(s"$stamp: $i Man")})
    def Women = for (i<-0 until N)   
             fork(proc (s"Woman$i"){ matching.womanEnter; println(s"$stamp: $i Woman")})
    if (args contains "-D") println(debugger)
    if (args contains "-HO") { Men; sleep(3*Sec); Women } // spawn (many) HS first
    else 
    if (args contains "-OH") { Men; sleep(3*Sec); Women } // spawn (many) OS first
    else  run(proc { Men } || proc { Women })             // interleave spawning
  }
}
