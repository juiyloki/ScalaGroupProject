class Test {

  /**
   * Runs the Sudoku generation test.
   * This will:
   * 1. Create a SudokuMaker for 9x9 boards.
   * 2. Define a list of difficulties (number of empty squares).
   * 3. For each difficulty, generate a full solution.
   * 4. Remove the specified number of squares to create a puzzle.
   * 5. Print the resulting puzzle board.
   */
  def test(N: Int): Unit = {

    // Instantiate the maker for 9x9 boards (N=9).
    // The 'difficulty' param isn't used by your methods, so '1' is a placeholder.
    // The passed 'N' parameter is also unused, hardcoding to 9.
    val maker = new SudokuMaker(difficulty = 1, N = 9)

    // List of puzzles to generate, specified by number of empty squares
    val emptySquareCounts = List(4, 6, 16, 20, 55)

    println("🚀 Starting Sudoku puzzle generation...\n")

    emptySquareCounts.foreach { count =>
      println(s"--- 🧩 Generating Puzzle with $count empty squares ---")

      try {
        // 1. Generate a full, valid solution board
        val fullBoard = maker.randomValidBoardGenerator()

        // 2. Remove 'count' squares to create the puzzle
        val puzzleBoard = maker.squareRemover(fullBoard, count)

        // 3. Print the puzzle to the console
        puzzleBoard.printBoard() // <-- Changed from printBoard to print

        println("----------------------------------\n")

      } catch {
        case e: RuntimeException =>
          println(s"Error generating puzzle with $count empty squares: ${e.getMessage}")
          println("----------------------------------\n")
      }
    }
  }

}