import io.threadcso._

object GameOfLife {
    val N = 100                                         // Board size
    val board = generateBoard(N)                        // Board of size N*N
    val p = 5                                           // Number of workers
    val MAX_ITERATIONS = 500                            // Maximum numbers of iterations
    val display = new Display(N, board)                 // Window to display board

    // Synchronisation
    val barrier = new Barrier(p+1)                      // Barrier to sync p workers and 1 display worker


    def main(args: Array[String]) {
        // Create workers and run them concurrently
        val displayWorker = createDisplayWorker()
        val workers = || (for (i <- 0 until p) yield createWorker("Worker " + i, i))
        run(workers || displayWorker)


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

    def createWorker(name: String, id: Int) = proc {
        // Each worker is responsible for 'N / p' rows starting from row 'id'
        val numRows = N / p
        val startRow = id * numRows

        println(name + " is responsible for rows " + startRow + "-" + (startRow+numRows))

        var iteration = 0

        var finished = false

        while(!finished) {
            barrier.sync // Make sure they all have written their rows
            // Thread.sleep(50)

            val updatedRows = computeUpdatedRows(startRow, numRows)

            barrier.sync // Make sure they've all computed their updatedRows
            writeUpdatedRows(startRow, numRows, updatedRows)

            iteration += 1
            if (iteration >= MAX_ITERATIONS) {
                finished = true
            }
        }
    }

    def createDisplayWorker() = proc {
        var iteration = 0
        var finished = false
        while(!finished) {
            
            barrier.sync
            Thread.sleep(50) // ms
            display.draw
            iteration += 1
            println("Iteration: " + iteration)
            
            barrier.sync    

            if (iteration >= MAX_ITERATIONS) {
                finished = true
            }
        }
        
    }

    def computeUpdatedRows(startRow : Int, rowCount : Int): Array[Array[Boolean]] = {
        val updatedRows = Array.ofDim[Boolean](rowCount, N)

        // Compute new state of rows
        for (rowId <- 0 until rowCount) {
            for (colId <- 0 until N) {
                updatedRows(rowId)(colId) = computeNewState(rowId + startRow, colId)
            }
        }

        return updatedRows
    }

    def computeNewState(r: Int, c: Int) : Boolean = {
        val livingNeighbours = countLivingNeighbours(r,c)

        var newState = board(r)(c)

        // Conway's Game of Life
        if(livingNeighbours < 2 || livingNeighbours > 3) {
            newState = false
        } else if (livingNeighbours == 3) {
            newState = true
        }

        return newState
    }

    def countLivingNeighbours(r: Int, c: Int) : Int = {
        var livingNeighbours = 0

        // TOP
        if (r > 0) {
            if (c > 0 && board(r-1)(c-1)) { livingNeighbours += 1}
            if (board(r-1)(c)) { livingNeighbours += 1}
            if (c < N - 1 && board(r-1)(c+1)) { livingNeighbours += 1}
        }
        // LEFT AND RIGHT
        if (c > 0 && board(r)(c-1)) { livingNeighbours += 1}
        if (c < N - 1 && board(r)(c+1)) { livingNeighbours += 1}

        // BOTTOM
        if (r < N - 1) {
            if (c > 0 && board(r+1)(c-1)) { livingNeighbours += 1}
            if (board(r+1)(c)) { livingNeighbours += 1}
            if (c < N - 1 && board(r+1)(c+1)) { livingNeighbours += 1}
        }

        livingNeighbours
    }

    def writeUpdatedRows(startRow : Int, rowCount : Int, updatedRows: Array[Array[Boolean]]) = synchronized {
        for (rowId <- startRow until (startRow + rowCount)) {
            for (colId <- 0 until N) {
                board(rowId)(colId) = updatedRows(rowId - startRow)(colId)
            }
        }
    }
}