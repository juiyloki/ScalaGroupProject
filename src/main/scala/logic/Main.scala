package logic

import scala.io.StdIn
object Main {

  def main(args: Array[String]): Unit = {

    val myTest = new Test()
    myTest.test(1)

    println("✅ Puzzle generation complete.")

    // Test gameplay in the console
    val game = new SudokuGame(N = 9)
    game.run()
  }
}
