class Board(private val N: Int, private val board: Vector[Vector[Square]]) {

  def this(N: Int) = {
    this(N, Vector.fill(N, N)(new Square()))
  }
    
  def this() = {
    this(9)
  }

  def getSize: Int = N
  
  def getSquare(row: Int, col: Int): Int = {
    board(row)(col).getValue
  }
  
  private def changeSquare(row: Int, col: Int, newSquare: Square): Board = {    
    val newRow = board(row).updated(col, newSquare)
    val newBoard = board.updated(row, newRow)
    new Board(N, newBoard)
  }

  def changeValue(value: Int, row: Int, col: Int): Board = {
    val newSquare = board(row)(col).changeValue(value)
    changeSquare(row,col,newSquare)
  }
  
  def hideSquare(row: Int, col: Int): Board = {
    val newSquare = board(row)(col).hide
    changeSquare(row,col,newSquare)
  }
  
  def showSquare(row: Int, col: Int): Board = {
    val newSquare = board(row)(col).show
    changeSquare(row,col,newSquare)
  }

  def isValueEqual(other: Board): Boolean = {
    
    if (this.N != other.N) return false
    
    this.board.zip(other.board).forall { 
      
      case (thisRow, otherRow) =>
        thisRow.zip(otherRow).forall { 
          case (thisSquare, otherSquare) =>
          thisSquare.getValue == otherSquare.getValue
        }
        
    }
    
  }

  def printBoard(): Unit = {
    // Calculate the size of the inner boxes (e.g., 3 for a 9x9 board)
    val boxSize: Int = Math.sqrt(N).toInt

    for ((row, r) <- board.zipWithIndex) {

      // Print an empty line to separate the 3-row blocks
      if (r > 0 && r % boxSize == 0) {
        println()
      }

      // Convert the row of Squares into a row of Strings ("?" or number)
      val rowStrings = row.map { square =>
        if (square.getVisibility) square.getValue.toString
        else "?"
      }

      // Group the strings by boxSize (e.g., 3), join inner group with " ",
      // and join the groups themselves with "  " (double empty space)
      val rowString = rowStrings
        .grouped(boxSize)
        .map(_.mkString(" "))
        .mkString("  ")

      println(rowString)
    }
  }
  
}
