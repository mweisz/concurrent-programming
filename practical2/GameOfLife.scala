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
        for (_ <- 0 to ((N*N) / 3).toInt) {
            val x = rand.nextInt(N)
            val y = rand.nextInt(N)

            board(x)(y) = true
        }

        // addReplicator(board)


        board
    }

    def addReplicator(board: Array[Array[Boolean]] ) = {
        // Replicator
        var offset = 10
        board(offset)(offset+2) = true
        board(offset)(offset+3) = true
        board(offset)(offset+4) = true
        
        board(offset+1)(offset+1) = true
        board(offset+1)(offset+4) = true

        board(offset+2)(offset) = true
        board(offset+2)(offset+4) = true

        board(offset+3)(offset) = true
        board(offset+3)(offset+3) = true

        board(offset+4)(offset) = true
        board(offset+4)(offset+1) = true
        board(offset+4)(offset+2) = true
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
                // Game of Life
                updatedRows(rowId)(colId) = computeNewState(rowId + startRow, colId)

                // Seeds
                // updatedRows(rowId)(colId) = computeNewStateSeeds(rowId + startRow, colId)

                // Highlife
                // updatedRows(rowId)(colId) = computeNewStateHighlife(rowId + startRow, colId)
            }
        }

        return updatedRows
    }

    def computeNewState(r: Int, c: Int) : Boolean = {
        val livingNeighbours = countLivingNeighboursWrap(r,c)

        var newState = board(r)(c)

        // Conway's Game of Life
        if(livingNeighbours < 2 || livingNeighbours > 3) {
            newState = false
        } else if (livingNeighbours == 3) {
            newState = true
        }

        return newState
    }

    // OPTIONAL: 
    def computeNewStateSeeds(r: Int, c: Int) : Boolean = {
        val livingNeighbours = countLivingNeighboursWrap(r,c)

        var newState = board(r)(c)

        // Seeds
        if(!newState && livingNeighbours == 2) {
            newState = true
        } else {
            newState = false
        }

        return newState
    }

    def computeNewStateHighlife(r: Int, c: Int) : Boolean = {
        val livingNeighbours = countLivingNeighboursWrap(r,c)

        var newState = board(r)(c)

        var born = !newState && (livingNeighbours == 3 || livingNeighbours == 6)
        var survive = newState && (livingNeighbours == 2 || livingNeighbours == 3)

        if(born || survive) {
            newState = true
        } else {
            newState = false
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

    def countLivingNeighboursWrap(r: Int, c: Int) : Int = {
        var livingNeighbours = 0

        for (x <- r-1 to r+1) {
            for (y <- c-1 to c+1) {
                if (!(x == r && y == c)) {
                    if (board((x+N)%N)((y+N)%N)) {
                        livingNeighbours += 1
                    }
                }
            }
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