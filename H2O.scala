import io.threadcso._

trait H2O {
    def H: Unit
    def O: Unit
}

class H2OJVM extends H2O {
    private val monitor = new Monitor
    private var hs, os = 0
    private var hsMayBond = 0

    private val hHasArrived, hMayBond = monitor.newCondition

    def H: Unit = monitor.withLock {
        hs += 1

        // Notify (potentially) waiting O's that new H has arrived
        hHasArrived.signal()

        // Wait until it may bond
        while (hsMayBond == 0) hMayBond.await()

        hsMayBond -= 1
        hs -= 1
    }

    def O: Unit = monitor.withLock { 
        os += 1

        // Wait until we have two Hs and one O
        while (!(hs >= 2 && os >= 1)) hHasArrived.await()

        os -= 1

        // Two more hs are allowed to bond (i.e. continue)
        hsMayBond += 2

        // Notifiy them
        hMayBond.signalAll()
    }
}

object H2OJVMTest extends H2OTest(new H2OJVM)



// Test Code
class H2OTest(h2o: H2O) { 
    def main(args: Array[String]) = { 
    val N  = if (args.size>0&&args(0).matches("[0-9]+")) args(0).toInt else 200
    val start = nanoTime
    def stamp = nanoTime-start
    def HS = for (i<-0 until 2*N) 
             fork(proc (s"H$i"){ h2o.H; println(s"$stamp: $i H")})
    def OS = for (i<-0 until N)   
             fork(proc (s"O$i"){ h2o.O; println(s"$stamp: $i O")})
    if (args contains "-D") println(debugger)
    if (args contains "-HO") { HS; sleep(3*Sec); OS } // spawn (many) HS first
    else 
    if (args contains "-OH") { OS; sleep(3*Sec); HS } // spawn (many) OS first
    else  run(proc { OS } || proc { HS })             // interleave spawning
  }
}