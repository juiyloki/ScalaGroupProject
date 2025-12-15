import javafx.scene.layout.{GridPane, VBox}
import javafx.application.Application
import javafx.scene.Scene
import javafx.scene.control.*
import javafx.stage.Stage
import javafx.geometry.Insets
import javafx.scene.control.Alert
import javafx.scene.control.Alert.AlertType
import javafx.application.Platform
import logic.{Board, SudokuMaker, SudokuSolver}

object Main {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[SudokuApp], args: _*)
  }
}

case class Pos(x: Int, y: Int)

// Class for buttons inside the grid
class CellButton(val pos: Pos, initial: Int) extends Button {
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

  // Value inputed by user are blue if they are valid, red otherwise
  private def setButtonTextColor(btn: CellButton, isValid: Boolean): Unit = {
    btn.getStyleClass.removeAll("btn-red", "btn-blue")
    btn.getStyleClass.add(if (isValid) "btn-blue" else "btn-red")
  }

  private def trySetValue(value: Int): Unit =
    gameState.selectedCell.foreach { btn =>
      if (btn.isMutable) {
        val Pos(x, y) = btn.pos

        val isMoveValid = solver.isValid(gameState.board, x, y, value)

        setButtonTextColor(btn, isMoveValid)
        btn.setValue(value)
        gameState.board = gameState.board.changeValue(value, x, y)

        checkForWin()
      }
    }

  private def checkForWin() :Unit= {
    if (gameState.board.isValueEqual(gameState.fullBoard)) {
      showWinDialog()
    }
  }

  private def showWinDialog(): Unit = {
    val alert = new Alert(AlertType.CONFIRMATION)
    alert.setHeaderText("Congratulations, you win.")
    alert.setContentText("Play again?")
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


  private def createGameScene(): Scene = {
    val boardFX = new GridPane()
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
        Pos(i, j),
        gameState.board.getSquare(i, j)
      )
      cell.setMaxSize(Double.MaxValue, Double.MaxValue)
      cell.setPrefSize(50, 50)
      cell.setOnAction(_ => selectButton(cell))

      boardFX.add(cell, j, i)
    }

    for (i <- 1 to 9) {
      val button = new Button(i.toString)
      button.setOnAction(_ => trySetValue(i))
      numButtons.add(button, i - 1, 0)
    }

    val eraseButton = new Button("X")
    eraseButton.setOnAction(_ => trySetValue(0))
    numButtons.add(eraseButton, 9, 0)

    val root = new VBox(10, boardFX, numButtons)
    root.setPadding(new Insets(20))

    val scene = new Scene(root, 500, 500)
    scene.getStylesheets.add(
      getClass.getResource("/css/style.css").toExternalForm
    )
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