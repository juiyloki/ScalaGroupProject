package logic

import scala.io.StdIn

/** Handles a console-based Sudoku gameplay session. */
class SudokuGame(N: Int = 9) {

  private val solver = new SudokuSolver

  /** Starts an interactive game session. */
  def run(): Unit = {
    println("=== Sudoku ===")
    println("Select difficulty: 1 – easy, 3 – medium, 5 – hard")
    val diffInput = StdIn.readLine("Difficulty (1 - 5): ")

    val difficulty =
      Option(diffInput)
        .flatMap(_.trim.toIntOption)
        .map(n => n.max(1).min(5))
        .getOrElse(3) // default

    val maker = new SudokuMaker(difficulty, N)
    println(s"Generating puzzle (difficulty = $difficulty)...")

    val puzzle: Board = maker.generatePuzzle()
    val current: Board = puzzle

    gameLoop(current, puzzle)
  }

  /** Main game loop: reads commands and updates board state. */
  private def gameLoop(currentBoard: Board, originalPuzzle: Board): Unit = {
    var board = currentBoard
    var exit  = false

    while (!exit) {
      println()
      board.printBoard()
      println()
      println("Enter move: `r c v` (row col value 1–9)")
      println("Commands: `solve`, `reset`, `quit`")
      print("> ")

      val input = StdIn.readLine()

      input match {
        case null =>
          exit = true

        case cmd if cmd.trim.equalsIgnoreCase("quit") =>
          println("Exiting game.")
          exit = true

        case cmd if cmd.trim.equalsIgnoreCase("reset") =>
          println("Restoring original puzzle.")
          board = originalPuzzle

        case cmd if cmd.trim.equalsIgnoreCase("solve") =>
          println("Solving...")
          solver.brutSolve(board) match {
            case Some(solved) =>
              val revealed = solved.revealAll()
              revealed.printBoard()
              println("Done. (Returning to gameplay with solved board.)")
              board = revealed
            case None =>
              println("This puzzle has no solution.")
          }

        case other =>
          val parts = other.trim.split("\\s+")
          if (parts.length != 3) {
            println("Invalid format. Use: r c v (e.g., `1 3 9`).")
          } else {
            val maybeMove = parseMove(parts)
            maybeMove match {
              case Some((r, c, v)) =>
                val row = r - 1
                val col = c - 1

                if (row < 0 || row >= board.getSize || col < 0 || col >= board.getSize) {
                  val temp = board.getSize
                  println(s"Out of bounds (valid: 1–$temp).")
                } else if (!board.isHidden(row, col)) {
                  println("Cannot modify this square (original puzzle value).")
                } else if (!solver.isValid(board, row, col, v)) {
                  println("Move violates Sudoku rules.")
                } else {
                  board = board.setUserValue(v, row, col)
                }

              case None =>
                println("Failed to parse move. Use e.g. `1 3 9`.")
            }
          }
      }
    }
  }

  /** Parses a move of the form (r, c, v). */
  private def parseMove(parts: Array[String]): Option[(Int, Int, Int)] = {
    try {
      val r = parts(0).toInt
      val c = parts(1).toInt
      val v = parts(2).toInt
      Some((r, c, v))
    } catch {
      case _: NumberFormatException => None
    }
  }
}
