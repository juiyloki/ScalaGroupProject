import javafx.scene.layout.{GridPane, VBox}
import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.control.*
import javafx.stage.Stage
import javafx.geometry.Insets
import javafx.application.Platform
import javafx.animation.{Animation, KeyFrame, Timeline}
import javafx.util.Duration
import javafx.scene.control.Label
import javafx.geometry.Pos
import javafx.scene.layout.HBox
import logic.{Board, SudokuMaker, SudokuSolver}

object Main {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[SudokuApp], args: _*)
  }
}

case class CellPos(x: Int, y: Int)

// Class for buttons inside the grid
class CellButton(val pos: CellPos, initial: Int) extends Button {
  // Only buttons that are not given an initial value are mutable
  var isMutable: Boolean = initial == 0

  def setValue(value: Int): Unit =
    setText(if (value == 0) "" else value.toString)

  setValue(initial)
}

class GameState(difficulty: Int) {
  private val solver = SudokuSolver()
  private val maker = SudokuMaker(difficulty, N = 9)

  // We keep track of the last cell selected by user.
  var selectedCell: Option[CellButton] = None

  var board: Board = maker.generatePuzzle()

  // History of boards for Undo functionality
  var history: List[Board] = Nil

  // Lives System:
  // Difficulty 1 (Very Easy) -> 5 lives
  // Difficulty 5 (Very Hard) -> 1 life
  val maxLives: Int = 6 - difficulty
  var mistakes: Int = 0

  // At this the board always has solution.
  val fullBoard: Board = solver.brutSolve(board).orNull
}

class SudokuApp extends Application {
  private var gameState: GameState = _
  private val solver = SudokuSolver()
  private var primaryStage: Stage = _
  private var cells: Vector[CellButton] = Vector.empty
  private var elapsedSeconds: Int = 0
  private var timerLabel: Label = _
  private var timeline: Timeline = _
  private var selectedDifficulty: Int = 3
  private var livesLabel: Label = _

  private def formatTime(totalSeconds: Int): String = {
    val minutes = totalSeconds / 60
    val seconds = totalSeconds % 60
    f"$minutes%02d:$seconds%02d"
  }

  private def startTimer(): Unit = {
    stopTimer()

    elapsedSeconds = 0
    timerLabel.setText(formatTime(elapsedSeconds))

    timeline = new Timeline(
      new KeyFrame(Duration.seconds(1), _ => {
        elapsedSeconds += 1
        timerLabel.setText(formatTime(elapsedSeconds))
      })
    )
    timeline.setCycleCount(Animation.INDEFINITE)
    timeline.play()
  }

  private def stopTimer(): Unit = {
    if (timeline != null) timeline.stop()
  }

  private def giveHint(): Unit = {
    val emptyCells = cells.filter { c =>
      val txt = c.getText
      (txt == null || txt.isEmpty)
    }

    if (emptyCells.isEmpty) return

    val chosen = emptyCells(scala.util.Random.nextInt(emptyCells.length))
    val CellPos(x, y) = chosen.pos

    val correctValue = gameState.fullBoard.getSquare(x, y)

    chosen.setValue(correctValue)
    chosen.getStyleClass.removeAll("btn-red", "btn-blue")
    chosen.getStyleClass.add("btn-blue")

    chosen.isMutable = false
    if (!chosen.getStyleClass.contains("cell-given")) {
      chosen.getStyleClass.add("cell-given")
    }

    gameState.board = gameState.board.changeValue(correctValue, x, y)

    val rowGroup = rowCells(x)
    if (isComplete(rowGroup)) flashCompleted(rowGroup)

    val colGroup = colCells(y)
    if (isComplete(colGroup)) flashCompleted(colGroup)

    val blockGroup = blockCells(x, y)
    if (isComplete(blockGroup)) flashCompleted(blockGroup)

    highlightSameNumbers(correctValue)
    checkForWin()
  }

  // Value inputed by user are blue if they are valid, red otherwise
  private def setButtonTextColor(btn: CellButton, isValid: Boolean): Unit = {
    btn.getStyleClass.removeAll("btn-red", "btn-blue")
    btn.getStyleClass.add(if (isValid) "btn-blue" else "btn-red")
  }

  private def trySetValue(value: Int): Unit =
    gameState.selectedCell.foreach { btn =>
      if (btn.isMutable) {
        val CellPos(x, y) = btn.pos

        val current = gameState.board.getSquare(x, y)

        // If we are changing the value, save the current board to history
        if (current != value) {
          gameState.history = gameState.board :: gameState.history
        }

        val isMoveValid =
          (value == 0) || (value == current) || solver.isValid(gameState.board, x, y, value)

        // If value is not 0 (clearing) and move is invalid, it's a mistake
        if (value != 0 && !isMoveValid) {
          gameState.mistakes += 1
          updateLivesLabel()
          
          if (gameState.mistakes >= gameState.maxLives) {
            stopTimer()
            primaryStage.setScene(createGameOverScene())
            return // Stop here, game is over
          }
        }

        setButtonTextColor(btn, isMoveValid)
        btn.setValue(value)
        gameState.board = gameState.board.changeValue(value, x, y)

        // Highlighting when row/column/group complete
        val rowGroup = rowCells(x)
        if (isComplete(rowGroup)) flashCompleted(rowGroup)

        val colGroup = colCells(y)
        if (isComplete(colGroup)) flashCompleted(colGroup)

        val blockGroup = blockCells(x, y)
        if (isComplete(blockGroup)) flashCompleted(blockGroup)

        checkForWin()
        // reset highlight for current cell
        val selectedValue = btn.getText match {
          case null | "" => 0
          case s => s.toInt
        }
        highlightSameNumbers(selectedValue)
      }
    }

  private def cellAt(row: Int, col: Int): CellButton =
    cells.find(_.pos == CellPos(row, col)).get

  private def rowCells(row: Int): Vector[CellButton] =
    (0 until 9).map(col => cellAt(row, col)).toVector

  private def colCells(col: Int): Vector[CellButton] =
    (0 until 9).map(row => cellAt(row, col)).toVector

  private def blockCells(row: Int, col: Int): Vector[CellButton] = {
    val r0 = (row / 3) * 3
    val c0 = (col / 3) * 3
    (for {
      r <- r0 until (r0 + 3)
      c <- c0 until (c0 + 3)
    } yield cellAt(r, c)).toVector
  }

  private def isComplete(group: Vector[CellButton]): Boolean = {
    val values = group.map { c =>
      val t = c.getText
      if (t == null || t.isEmpty) 0 else t.toInt
    }
    if (values.contains(0)) return false
    values.toSet == (1 to 9).toSet
  }

  private def flashCompleted(group: Vector[CellButton]): Unit = {
    group.foreach(_.getStyleClass.add("completed-group"))
    val t = new javafx.animation.PauseTransition(javafx.util.Duration.millis(400))
    t.setOnFinished(_ => group.foreach(_.getStyleClass.remove("completed-group")))
    t.play()
  }

  private def highlightLine(row: Int, col: Int): Unit = {
    cells.foreach { c =>
      c.getStyleClass.removeAll("cell-line-empty", "cell-line-filled")
    }

    cells.foreach { c =>
      val CellPos(r, ccol) = c.pos
      if (r == row || ccol == col) {
        val txt = c.getText
        val isEmpty = (txt == null || txt.isEmpty)

        if (isEmpty) c.getStyleClass.add("cell-line-empty")
        else c.getStyleClass.add("cell-line-filled")
      }
    }
  }

  private def checkForWin(): Unit = {
    if (gameState.board.isValueEqual(gameState.fullBoard)) {
      stopTimer()
      primaryStage.setScene(createVictoryScene(elapsedSeconds))
    }
  }

  private def selectButton(button: CellButton): Unit = {
    gameState.selectedCell.foreach { btn =>
      btn.getStyleClass.remove("btn-selected")
    }
    gameState.selectedCell = Some(button)
    button.getStyleClass.add("btn-selected")
  }

  private def highlightSameNumbers(value: Int): Unit = {
    cells.foreach(_.getStyleClass.remove("cell-highlight"))

    if (value == 0) return

    cells.foreach { c =>
      val current = c.getText match {
        case null | "" => 0
        case s => s.toInt
      }
      if (current == value) c.getStyleClass.add("cell-highlight")
    }
  }

  private def setSudokuBorders(cell: Button, row: Int, col: Int): Unit = {
    val thin = 0.5
    val thick = 2.0

    val top = if (row % 3 == 0) thick else thin
    val left = if (col % 3 == 0) thick else thin
    val bottom = if ((row + 1) % 3 == 0) thick else thin
    val right = if ((col + 1) % 3 == 0) thick else thin

    cell.setStyle(s"-fx-border-color: #444; -fx-border-width: $top $right $bottom $left;")
  }

  private def createGameScene(): Scene = {
    val boardFX = new GridPane()
    cells = Vector.empty
    boardFX.getStyleClass.add("board")
    boardFX.setHgap(2)
    boardFX.setVgap(2)
    boardFX.setPadding(new Insets(10))

    val numButtons = new GridPane()
    numButtons.setHgap(5)
    numButtons.setVgap(5)
    numButtons.setPadding(new Insets(10))

    for (_ <- 0 until 9) {
      val col = new javafx.scene.layout.ColumnConstraints()
      col.setPercentWidth(100.0 / 9)
      boardFX.getColumnConstraints.add(col)

      val row = new javafx.scene.layout.RowConstraints()
      row.setPercentHeight(100.0 / 9)
      boardFX.getRowConstraints.add(row)
    }

    for (i <- 0 until 9; j <- 0 until 9) {
      val cell = new CellButton(
        CellPos(i, j),
        gameState.board.getSquare(i, j)
      )
      cell.setMaxSize(Double.MaxValue, Double.MaxValue)
      cell.setPrefSize(50, 50)
      cells = cells :+ cell

      // CSS look
      cell.getStyleClass.add("cell")
      if (!cell.isMutable) cell.getStyleClass.add("cell-given")

      // lines for sections of 3×3
      setSudokuBorders(cell, i, j)

      cell.setOnAction(_ => {
        selectButton(cell)
        highlightLine(cell.pos.x, cell.pos.y)

        val value = cell.getText match {
          case null | "" => 0
          case s => s.toInt
        }
        highlightSameNumbers(value)
      })

      boardFX.add(cell, j, i)
    }

    for (i <- 1 to 9) {
      val button = new Button(i.toString)
      button.getStyleClass.add("key")
      button.setOnAction(_ => trySetValue(i))
      numButtons.add(button, i - 1, 0)
    }

    val eraseButton = new Button("X")
    eraseButton.getStyleClass.add("key")
    eraseButton.setOnAction(_ => trySetValue(0))
    numButtons.add(eraseButton, 9, 0)

    val undoButton = new Button("Undo")
    undoButton.getStyleClass.addAll("key") 
    undoButton.setOnAction(_ => undo())
    numButtons.add(undoButton, 10, 0)

    val hintButton = new Button("Hint")
    hintButton.getStyleClass.addAll("key", "hint")
    hintButton.setOnAction(_ => giveHint())
    numButtons.add(hintButton, 11, 0)

    val menuButton = new Button("Menu")
    menuButton.getStyleClass.add("key")
    menuButton.setOnAction(_ => {
      stopTimer()
      primaryStage.setScene(createMenuScene())
    })
    numButtons.add(menuButton, 0, 1, 12, 1)

    timerLabel = new Label("00:00")
    timerLabel.getStyleClass.add("timer")
    livesLabel = new Label("")
    livesLabel.getStyleClass.add("timer") 
    updateLivesLabel()
    val topBar = new HBox(20, livesLabel, timerLabel)
    topBar.setAlignment(Pos.TOP_RIGHT)

    val root = new VBox(10, topBar, boardFX, numButtons)
    root.setPadding(new Insets(20))

    val scene = new Scene(root, 600, 650)
    scene.getStylesheets.add(
      getClass.getResource("/css/style.css").toExternalForm
    )
    startTimer()
    scene
  }

  private def createMenuScene(): Scene = {
    val title = new Label("Sudoku")
    title.getStyleClass.add("menu-title")

    val subtitle = new Label("Choose difficulty:")
    subtitle.getStyleClass.add("menu-subtitle")
    val tg = new javafx.scene.control.ToggleGroup()

    def mkRadio(text: String, value: Int): RadioButton = {
      val rb = new RadioButton(text)
      rb.setToggleGroup(tg)
      rb.getStyleClass.add("menu-radio")
      rb.setOnAction(_ => selectedDifficulty = value)
      rb
    }

    val veryEasy = mkRadio("Very Easy", 1)
    val easy = mkRadio("Easy", 2)
    val medium = mkRadio("Medium", 3)
    val hard = mkRadio("Hard", 4)
    val veryHard = mkRadio("Very Hard", 5)

    medium.setSelected(true)
    val difficultyBox = new VBox(8, veryEasy, easy, medium, hard, veryHard)
    difficultyBox.getStyleClass.add("menu-difficulty")

    val playBtn = new Button("Play")
    playBtn.getStyleClass.addAll("key", "menu-button")
    playBtn.setPrefWidth(200)
    playBtn.setOnAction(_ => {
      gameState = new GameState(selectedDifficulty)
      primaryStage.setScene(createGameScene())
    })

    val rulesBtn = new Button("Rules")
    rulesBtn.getStyleClass.addAll("key", "menu-button")
    rulesBtn.setPrefWidth(200)
    rulesBtn.setOnAction(_ => {
      primaryStage.setScene(createRulesScene())
    })

    val buttons = new HBox(12, playBtn, rulesBtn)
    buttons.setAlignment(Pos.CENTER)

    val root = new VBox(18, title, subtitle, difficultyBox, buttons)
    root.setPadding(new Insets(30))
    root.setAlignment(Pos.CENTER)

    val scene = new Scene(root, 600, 650)
    scene.getStylesheets.add(getClass.getResource("/css/style.css").toExternalForm)
    scene
  }

  private def createRulesScene(): Scene = {
    val title = new Label("Rules")
    title.getStyleClass.add("menu-title")

    val rulesText = new Label(
      "• Fill the grid so that every row and every column contains digits 1–9 without repetitions.\n" +
        "• Every 3×3 block must contain digits 1–9 without repetitions.\n" +
        "• Each puzzle has one unique solution.\n\n" +
        "• You cannot change the given (gray) numbers.\n" +
        "• Use Hint to fill one cell (if available).\n" +
        "• X removes a number from the selected cell."
    )
    rulesText.getStyleClass.add("rules-text")
    rulesText.setWrapText(true)

    val backBtn = new Button("Back")
    backBtn.getStyleClass.addAll("key", "menu-button")
    backBtn.setPrefWidth(200)
    backBtn.setOnAction(_ => primaryStage.setScene(createMenuScene()))

    val root = new VBox(18, title, rulesText, backBtn)
    root.setPadding(new Insets(30))
    root.setAlignment(Pos.CENTER)

    val scene = new Scene(root, 600, 650)
    scene.getStylesheets.add(getClass.getResource("/css/style.css").toExternalForm)
    scene
  }

  private def createVictoryScene(timeSeconds: Int): Scene = {
    val title = new Label("You won!")
    title.getStyleClass.add("menu-title")

    val timeLbl = new Label(s"Time: ${formatTime(timeSeconds)}")
    timeLbl.getStyleClass.add("victory-time")

    val playAgainBtn = new Button("Play again")
    playAgainBtn.getStyleClass.addAll("key", "menu-button")
    playAgainBtn.setPrefWidth(200)
    playAgainBtn.setOnAction(_ => {
      gameState = new GameState(selectedDifficulty) // same difficulty
      primaryStage.setScene(createGameScene())
    })

    val menuBtn = new Button("Menu")
    menuBtn.getStyleClass.addAll("key", "menu-button")
    menuBtn.setPrefWidth(200)
    menuBtn.setOnAction(_ => primaryStage.setScene(createMenuScene()))

    val exitBtn = new Button("Exit")
    exitBtn.getStyleClass.addAll("key", "menu-button")
    exitBtn.setPrefWidth(200)
    exitBtn.setOnAction(_ => Platform.exit())

    val buttons = new HBox(12, playAgainBtn, menuBtn, exitBtn)
    buttons.setAlignment(Pos.CENTER)

    val root = new VBox(18, title, timeLbl, buttons)
    root.setPadding(new Insets(30))
    root.setAlignment(Pos.CENTER)

    val scene = new Scene(root, 600, 650)
    scene.getStylesheets.add(getClass.getResource("/css/style.css").toExternalForm)
    scene
  }

  override def start(stage: Stage): Unit = {
    primaryStage = stage
    stage.setTitle("Sudoku")
    stage.setScene(createMenuScene())
    stage.show()
  }

  // Undo functionality

  private def undo(): Unit = {
    if (gameState.history.nonEmpty) {
      // Restore the previous board
      gameState.board = gameState.history.head
      gameState.history = gameState.history.tail
      
      // Refresh the UI to match the restored board
      refreshGrid()
    }
  }

  private def refreshGrid(): Unit = {
    cells.foreach { btn =>
      val CellPos(r, c) = btn.pos
      val value = gameState.board.getSquare(r, c)
      
      // Update text
      btn.setValue(value)
      
      // Reset colors (remove red/blue/highlight) so they don't stick around
      btn.getStyleClass.removeAll("btn-red", "btn-blue", "cell-highlight")
    }
    // Update lives label just in case
    updateLivesLabel()
  }

  private def updateLivesLabel(): Unit = {
    if (livesLabel != null) {
      val remaining = gameState.maxLives - gameState.mistakes
      livesLabel.setText(s"Lives: $remaining/${gameState.maxLives}")
    }
  }

  // Defeat menu

  private def createGameOverScene(): Scene = {
    val title = new Label("Game Over")
    title.getStyleClass.add("menu-title") // Reusing menu-title style

    val msg = new Label("You ran out of lives!")
    msg.setStyle("-fx-font-size: 18px; -fx-text-fill: white;")

    val menuBtn = new Button("Back to Menu")
    menuBtn.getStyleClass.addAll("key", "menu-button")
    menuBtn.setPrefWidth(200)
    menuBtn.setOnAction(_ => primaryStage.setScene(createMenuScene()))

    val exitBtn = new Button("Exit")
    exitBtn.getStyleClass.addAll("key", "menu-button")
    exitBtn.setPrefWidth(200)
    exitBtn.setOnAction(_ => Platform.exit())

    val root = new VBox(20, title, msg, menuBtn, exitBtn)
    root.setPadding(new Insets(30))
    root.setAlignment(Pos.CENTER)
    
    // Reusing existing CSS
    val scene = new Scene(root, 600, 650)
    scene.getStylesheets.add(getClass.getResource("/css/style.css").toExternalForm)
    scene
  }

}