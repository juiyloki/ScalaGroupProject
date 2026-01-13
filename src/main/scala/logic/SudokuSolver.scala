package logic

import scala.util.Random // Import for shuffling

class SudokuSolver {

  /**
   * Solves a board deterministically (trying 1..N in order).
   * Returns the first solution found.
   */
  def brutSolve(board: Board): Option[Board] = {
    findEmptySquare(board) match {
      case None =>
        Some(board) // Solved

      case Some((row, col)) =>
        (1 to board.N).to(LazyList).flatMap { num =>
          if (isValid(board, row, col, num)) {
            brutSolve(board.changeValue(num, row, col))
          } else {
            LazyList.empty
          }
        }.headOption
    }
  }

  /**
   * Solves a board by trying 1..N in a *random* order at each step.
   * This is used to generate varied, fully-filled boards.
   */
  def randomizedSolve(board: Board): Option[Board] = {
    findEmptySquare(board) match {
      case None =>
        Some(board) // Solved

      case Some((row, col)) =>
        // The only change: shuffle the numbers 1-N
        Random.shuffle(1 to board.N).to(LazyList).flatMap { num =>
          if (isValid(board, row, col, num)) {
            randomizedSolve(board.changeValue(num, row, col))
          } else {
            LazyList.empty
          }
        }.headOption
    }
  }

  /**
   * Counts the number of solutions for a given board.
   * Stops counting once `maxCount` is reached.
   * To check for uniqueness, call `countSolutions(board, 2)`.
   * - If it returns 0: No solution
   * - If it returns 1: Unique solution
   * - If it returns 2: More than one solution
   */
  def countSolutions(board: Board, maxCount: Int): Int = {
    findEmptySquare(board) match {
      case None =>
        1 // A complete, valid board is 1 solution

      case Some((row, col)) =>
        // Use foldLeft to sum up solutions from child branches
        (1 to board.N).foldLeft(0) { (count, num) =>
          if (count >= maxCount) {
            // Short-circuit: We've already found enough solutions
            count
          } else {
            if (isValid(board, row, col, num)) {
              // Add solutions from this valid branch
              count + countSolutions(board.changeValue(num, row, col), maxCount - count)
            } else {
              // This number is invalid, add 0 solutions
              count
            }
          }
        }
    }
  }


  private def findEmptySquare(board: Board): Option[(Int, Int)] = {
    val N: Int = board.N
    val allCoordinates = (0 until N).view.flatMap { r =>
      (0 until N).view.map { c =>
        (r, c)
      }
    }
    allCoordinates.find { case (r, c) =>
      board.getSquare(r, c) == 0
    }
  }

  // --- Public Helper Methods (Needed by SudokuMaker) ---

  def isValid(board: Board, row: Int, col: Int, num: Int): Boolean = {
    //    val N: Int = board.N
    //    val boxSize: Int = Math.sqrt(N).toInt
    val boxStartRow: Int = row - row % board.boxHeight
    val boxStartCol: Int = col - col % board.boxWidth

    // Check if num is 0 (which isn't valid) or if it's already in a row/col/box
    num != 0 &&
      !isInRow(board, row, num) &&
      !isInCol(board, col, num) &&
      !isInBox(board, boxStartRow, boxStartCol, num)
  }

  def isInRow(board: Board, row: Int, num: Int): Boolean = {
    (0 until board.N).exists(col => board.getSquare(row, col) == num)
  }

  def isInCol(board: Board, col: Int, num: Int): Boolean = {
    (0 until board.N).exists(row => board.getSquare(row, col) == num)
  }

  def isInBox(board: Board, startRow: Int, startCol: Int, num: Int): Boolean = {
    val boxSize: Int = Math.sqrt(board.N).toInt
    (0 until boxSize).exists { r =>
      (0 until boxSize).exists { c =>
        board.getSquare(startRow + r, startCol + c) == num
      }
    }
  }
}