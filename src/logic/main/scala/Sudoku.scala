class Sudoku {

  //def generate(): Board = {}
  //private def randomValidBoard(): Board = {}

  def solve(board: Board): Board = {
    val solver = new SudokuSolver
    solver.brutSolve(board) match {
      case Some(solvedBoard) => solvedBoard
      case None =>
        println("No solution exists for this board.")
        board
    }
  }

  

}