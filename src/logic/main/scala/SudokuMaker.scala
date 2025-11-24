import scala.util.Random
import scala.annotation.tailrec

class SudokuMaker(private val difficulty: Int, private val N: Int) {

  private val solver = new SudokuSolver

  /** Map difficulty to the number of empty spaces */
  private def emptySquaresForDifficulty: Int = difficulty match {
    case 1 => 10  // very easy
    case 2 => 25  // easy
    case 3 => 35  // medium
    case 4 => 45  // hard
    case 5 => 55  // very hard
    case _ => 35  // default medium
  }

  /** Generates board, subtracts toRemove numbers accoring to set difficulty */
  def generatePuzzle(): Board = {
    val fullBoard = randomValidBoardGenerator()
    val toRemove  = emptySquaresForDifficulty
    squareRemover(fullBoard, toRemove)
  }

  /**
   * Generates a completely filled, valid Sudoku board.
   */
  def randomValidBoardGenerator(): Board = {
    solver.randomizedSolve(new Board(N)) match {
      case Some(fullBoard) => fullBoard
      case None =>
        throw new RuntimeException("Failed to generate a full board.")
    }
  }

  /**
   * Removes squares from a full board to create a puzzle.
   */
  def squareRemover(fullBoard: Board, num_of_squares_to_be_removed: Int): Board = {

    val allCoords = (0 until N).flatMap { r =>
      (0 until N).map { c =>
        (r, c)
      }
    }

    val shuffledCoords = Random.shuffle(allCoords.toList)

    @tailrec
    def removeHelper(currentBoard: Board, coordsToTry: List[(Int, Int)], removedCount: Int): Board = {

      // Stop conditions
      if (removedCount >= num_of_squares_to_be_removed) {
        // Success: We removed the target number of squares
        currentBoard

      } else if (coordsToTry.isEmpty) {
        // **CHANGE 1: Throw error if we failed to remove enough squares**
        // Failure: We tried all cells but couldn't remove the target amount
        throw new RuntimeException(
          s"Failed to remove $num_of_squares_to_be_removed squares. " +
            s"Only $removedCount could be removed while maintaining a unique solution."
        )

      } else {
        // Recursive step
        val (row, col) = coordsToTry.head
        val remainingCoords = coordsToTry.tail

        // **CHANGE 2: Use hideSquare AND changeValue(0, ...)**
        // We must set value to 0 for the solver's findEmptySquare(..)
        // We use hideSquare() so the board prints '?' instead of '0'
        val boardWithRemoval = currentBoard.changeValue(0, row, col).hideSquare(row, col)

        // Test uniqueness
        val solutionCount = solver.countSolutions(boardWithRemoval, 2)

        if (solutionCount == 1) {
          // Keep the removal and recurse
          removeHelper(boardWithRemoval, remainingCoords, removedCount + 1)
        } else {
          // Undo removal (by using the original board) and recurse
          removeHelper(currentBoard, remainingCoords, removedCount)
        }
      }
    }

    // Start the process
    removeHelper(fullBoard, shuffledCoords, 0)
  }

}