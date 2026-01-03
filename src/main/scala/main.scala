import javafx.scene.layout.{GridPane, VBox}
import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.control.*
import javafx.stage.Stage
import javafx.geometry.Insets
import javafx.scene.control.Alert
import javafx.scene.control.Alert.AlertType
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
  val isMutable: Boolean = initial == 0


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

  private def askDifficulty(): Int = {
    val choices = Map(
      "Very Easy (1)" -> 1,
      "Easy (2)" -> 2,
      "Medium (3)" -> 3,
      "Hard (4)" -> 4,
      "Very Hard (5)" -> 5
    )

    val orderedChoices = choices.keys.toSeq.sortBy(choices)
    val dialog = new ChoiceDialog[String]("Medium (3)", orderedChoices: _*)
    dialog.setTitle("Sudoku")
    dialog.setHeaderText("Choose difficulty")
    dialog.setContentText("Difficulty level:")
    val result = dialog.showAndWait()
    if (result.isPresent) {
      choices(result.get())
    } else {
      Platform.exit()
      0
    }
  }

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

  // Value inputed by user are blue if they are valid, red otherwise
  private def setButtonTextColor(btn: CellButton, isValid: Boolean): Unit = {
    btn.getStyleClass.removeAll("btn-red", "btn-blue")
    btn.getStyleClass.add(if (isValid) "btn-blue" else "btn-red")
  }

  private def trySetValue(value: Int): Unit =
    gameState.selectedCell.foreach { btn =>
      if (btn.isMutable) {
        val CellPos(x, y) = btn.pos

        val isMoveValid = solver.isValid(gameState.board, x, y, value)

        setButtonTextColor(btn, isMoveValid)
        btn.setValue(value)
        gameState.board = gameState.board.changeValue(value, x, y)

        checkForWin()
        // reset highlight for current cell
        val selectedValue = btn.getText match {
          case null | "" => 0
          case s => s.toInt
        }
        highlightSameNumbers(selectedValue)
      }
    }

  private def checkForWin() :Unit= {
    if (gameState.board.isValueEqual(gameState.fullBoard)) {
      showWinDialog()
    }
  }

  private def showWinDialog(): Unit = {
    stopTimer()
    val alert = new Alert(AlertType.CONFIRMATION)
    alert.setHeaderText("Congratulations, you win.")
    alert.setContentText(s"You solved it in ${formatTime(elapsedSeconds)}.\nPlay again?")
    alert.getButtonTypes.setAll(
      javafx.scene.control.ButtonType.YES,
      javafx.scene.control.ButtonType.NO
    )
    val result = alert.showAndWait()
    if (result.isPresent && result.get == javafx.scene.control.ButtonType.YES) {
      restartGame()
    }
  }

  private def restartGame(): Unit = {
    val difficulty = askDifficulty()
    gameState = new GameState(difficulty)
    primaryStage.setScene(createGameScene())
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

    timerLabel = new Label("00:00")
    timerLabel.getStyleClass.add("timer")
    val topBar = new HBox(timerLabel)
    topBar.setAlignment(Pos.TOP_RIGHT)

    val root = new VBox(10, topBar, boardFX, numButtons)
    root.setPadding(new Insets(20))

    val scene = new Scene(root, 500, 500)
    scene.getStylesheets.add(
      getClass.getResource("/css/style.css").toExternalForm
    )
    startTimer()
    scene
  }

  override def start(stage: Stage): Unit = {
    primaryStage = stage
    gameState = new GameState(askDifficulty())

    stage.setScene(createGameScene())
    stage.setTitle("Sudoku")
    stage.show()
  }
}