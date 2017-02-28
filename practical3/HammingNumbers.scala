import io.threadcso._
// import io.threadcso.altimp.channels

object HammingNumbers {
    // Prefix
    def prefix(i: Int, out: ![Int]) = proc { 
        out!i 
    }


    // Multiplication
    def mult(x: Int, in: ?[Int], out: ![Int]) = proc {
        repeat {
            Thread.sleep(10)
            out!(x * (in?))
        }

        // Close channels once interrupted
        in.closeIn
        out.closeOut

    }


    // Tee
    def tee(in: ?[Int], out1: ![Int], out2: ![Int]) = proc {
        repeat {
            var v : Int = in?;
            (proc { out1!v } || proc { out2!v })()
        }

        // Close channels once interrupted
        in.closeIn 
        out1.closeOut
        out2.closeOut
    }


    // Merge
    def merge(in1: ?[Int], in2: ?[Int], out: ![Int]) = proc {
        var x1 = in1?;
        var x2 = in2?;

        repeat {
            if (x1 == x2) {
                out!(x1)
                // drop x2 (because it is equal to x1)

                x1 = in1?;
                x2 = in2?;
            } else if (x1 < x2) {
                out!x1;
                x1 = in1?;
            } else { // x1 > x2
                out!x2;
                x2 = in2?;
            }
        }

        // Close channels once interrupted
        in1.closeIn 
        in2.closeIn 
        out.closeOut
    }


    // Printer
    def printer(in: ?[Int]) = proc {
        repeat {
            println(in?)
        }

        in.closeIn
        
    }

    def limit(in: ?[Int]) = proc {
        var n = 1

        while (n < 1000) {
            in?;
            n += 1
        }

        in.closeIn
    }


    def main(args: Array[String]) = {
        val pOut, t1Out1, t1Out2, t2Out1, t2Out2, t3Out1, t3Out2, limitCounter, hammingNumber  = OneOne[Int]
        val x2Out, x3Out, m1Out, x5Out  = OneOneBuf[Int](1024)

        // Compose processes
        val channel =   prefix(1, pOut)                             ||
                        tee(pOut, t1Out1, t1Out2)                   ||
                        tee(t1Out2, t2Out1, t2Out2)                 ||
                        mult(2, t2Out1, x2Out)                      ||
                        tee(t2Out2, t3Out1, t3Out2)                 ||
                        mult(3, t3Out1, x3Out)                      ||
                        mult(5, t3Out2, x5Out)                      ||
                        merge(x2Out, x3Out, m1Out)                  ||
                        merge(m1Out, x5Out, pOut)                   ||
                        tee(t1Out1, hammingNumber, limitCounter)    ||
                        limit(limitCounter)                         ||
                        printer(hammingNumber)    

        // Run processes
        channel()
    }
}