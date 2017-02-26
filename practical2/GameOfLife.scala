import io.threadcso._

object GameOfLife {


    def main(args: Array[String]) {
        // Initialise Board
        val N = 100
        val board = generateBoard(N)
        
        // Create display window for the board
        val display = new Display(N, board)

        // Create workers and run them concurrently
        val p = 3
        val workers = || (for (i <- 0 until p) yield createWorker("Worker " + i))
        run(workers)

    }

    def generateBoard(N : Int): Array[Array[Boolean]] = {
        val board = Array.ofDim[Boolean](N,N)
        
        // Random Init
        val rand = scala.util.Random
        for (_ <- 0 to 2000) {
            val x = rand.nextInt(N)
            val y = rand.nextInt(N)

            board(x)(y) = true
        }

        board
    }

    def createWorker(name: String) = proc {
        while(true) {
            Thread.sleep(1000)
            println(name)
        }
    }
}