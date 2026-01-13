import javafx.scene.layout.{GridPane, HBox, Pane, Priority, VBox}
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
import logic.{Board, SudokuMaker, SudokuSolver}
import javafx.scene.input.{KeyCode, KeyEvent}
import javafx.scene.control.Alert
import javafx.scene.control.Alert.AlertType
import javafx.scene.control.ButtonType
import javafx.scene.media.{Media, MediaPlayer, MediaView}

object Main {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[SudokuApp], args: _*)
  }
}

object GameConfig {
  var allowHints: Boolean = true
  var showTimer: Boolean = true
  var showLives: Boolean = true
  var musicEnabled: Boolean = false
  var pinkMode: Boolean = false
  var showVideo: Boolean = false
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

class GameState(difficulty: Int, N: Int) {

  val (boxHeight, boxWidth) = N match {
    case 9 => (3, 3)
    case 6 => (2, 3)
    case 4 => (2, 2)
    case _ => (3, 3)
  }

  private val solver = SudokuSolver()
  private val maker = SudokuMaker(difficulty, N, boxHeight, boxWidth)

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
  private var selectedSize: Int = 9
  private var selectedDifficulty: Int = 3
  private var livesLabel: Label = _
  private var mediaPlayer: MediaPlayer = _
  private var videoMediaPlayer: MediaPlayer = _

  private def formatTime(totalSeconds: Int): String = {
    val minutes = totalSeconds / 60
    val seconds = totalSeconds % 60
    f"$minutes%02d:$seconds%02d"
  }

  private def initMusic(): Unit = {
    try {
      val resource = getClass.getResource("/music/epic.mp3")
      if (resource != null) {
        val media = new Media(resource.toExternalForm)
        mediaPlayer = new MediaPlayer(media)
        mediaPlayer.setCycleCount(MediaPlayer.INDEFINITE)
        if (GameConfig.musicEnabled) mediaPlayer.play()
      } else {
        println("Music file not found! Check src/main/resources/music/background.mp3")
      }
    } catch {
      case e: Exception => println(s"Error loading music: ${e.getMessage}")
    }
  }

  private def initVideo(): Unit = {
    try {
      val resource = getClass.getResource("/video/focus.mp4")
      if (resource != null) {
        val media = new Media(resource.toExternalForm)
        videoMediaPlayer = new MediaPlayer(media)
        videoMediaPlayer.setCycleCount(MediaPlayer.INDEFINITE)
        videoMediaPlayer.setMute(true)
        if (GameConfig.showVideo) videoMediaPlayer.play()
      } else {
        println("Video file not found! Check src/main/resources/video/focus.mp4")
      }
    } catch {
      case e: Exception => println(s"Error loading video: ${e.getMessage}")
    }
  }

  private def startTimer(): Unit = {
    stopTimer()

    if (!GameConfig.showTimer) {
      timerLabel.setText("")
      return
    }
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
    if (!GameConfig.allowHints) return

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

  private def trySetValue(value: Int): Unit = {
    if (gameState.selectedCell.isEmpty) return
    val btn = gameState.selectedCell.get

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

        if (GameConfig.showLives && gameState.mistakes >= gameState.maxLives) {
          stopTimer()
          changeScene(createGameOverScene())
          return
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
    (0 until gameState.board.N).map(col => cellAt(row, col)).toVector

  private def colCells(col: Int): Vector[CellButton] =
    (0 until gameState.board.N).map(row => cellAt(row, col)).toVector

  private def blockCells(row: Int, col: Int): Vector[CellButton] = {
    val height = gameState.board.boxHeight
    val width = gameState.board.boxWidth
    val r0 = (row / height) * height
    val c0 = (col / width) * width
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
      changeScene(createVictoryScene(elapsedSeconds))
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

  private def handleCellSelection(cell: CellButton): Unit = {
    selectButton(cell)
    highlightLine(cell.pos.x, cell.pos.y)

    val value = cell.getText match {
      case null | "" => 0
      case s => s.toInt
    }
    highlightSameNumbers(value)
  }

  private def handleGameKeyPress(e: KeyEvent): Unit = {
    if (e.isControlDown && e.getCode == KeyCode.Z) {
      undo()
      e.consume()
      return
    }

    if (gameState.selectedCell.isDefined) {
      val current = gameState.selectedCell.get
      val CellPos(r, c) = current.pos
      e.getCode match {
        case KeyCode.UP    => if (r > 0) handleCellSelection(cellAt(r - 1, c)); e.consume()
        case KeyCode.DOWN  => if (r < 8) handleCellSelection(cellAt(r + 1, c)); e.consume()
        case KeyCode.LEFT  => if (c > 0) handleCellSelection(cellAt(r, c - 1)); e.consume()
        case KeyCode.RIGHT => if (c < 8) handleCellSelection(cellAt(r, c + 1)); e.consume()
        case _ =>
      }
    } else if (cells.nonEmpty) {
      e.getCode match {
        case KeyCode.UP | KeyCode.DOWN | KeyCode.LEFT | KeyCode.RIGHT =>
          handleCellSelection(cellAt(4, 4))
          e.consume()
        case _ =>
      }
    }

    e.getCode match {
      case KeyCode.DIGIT1 | KeyCode.NUMPAD1 => trySetValue(1); e.consume()
      case KeyCode.DIGIT2 | KeyCode.NUMPAD2 => trySetValue(2); e.consume()
      case KeyCode.DIGIT3 | KeyCode.NUMPAD3 => trySetValue(3); e.consume()
      case KeyCode.DIGIT4 | KeyCode.NUMPAD4 => trySetValue(4); e.consume()
      case KeyCode.DIGIT5 | KeyCode.NUMPAD5 => trySetValue(5); e.consume()
      case KeyCode.DIGIT6 | KeyCode.NUMPAD6 => trySetValue(6); e.consume()
      case KeyCode.DIGIT7 | KeyCode.NUMPAD7 => trySetValue(7); e.consume()
      case KeyCode.DIGIT8 | KeyCode.NUMPAD8 => trySetValue(8); e.consume()
      case KeyCode.DIGIT9 | KeyCode.NUMPAD9 => trySetValue(9); e.consume()
      case KeyCode.DIGIT0 | KeyCode.NUMPAD0 | KeyCode.BACK_SPACE | KeyCode.DELETE => trySetValue(0); e.consume()
      case KeyCode.H => giveHint(); e.consume()
      case KeyCode.M => confirmExitToMenu(); e.consume()
      case _ =>
    }
  }

  private def setSudokuBorders(cell: Button, row: Int, col: Int, boxHeight: Int, boxWidth: Int): Unit = {
    val thin = 0.5
    val thick = 2.0

    val top = if (row % boxHeight == 0) thick else thin
    val left = if (col % boxWidth == 0) thick else thin
    val bottom = if ((row + 1) % boxHeight == 0) thick else thin
    val right = if ((col + 1) % boxWidth == 0) thick else thin

    cell.setStyle(s"-fx-border-color: #444; -fx-border-width: $top $right $bottom $left;")
  }


  private def changeScene(scene: Scene): Unit = {
    // takes current dimensions to maintain window size
    val currentWidth = primaryStage.getWidth
    val currentHeight = primaryStage.getHeight

    primaryStage.setScene(scene)

    primaryStage.setWidth(currentWidth)
    primaryStage.setHeight(currentHeight)

    // sets minimum size so the window cannot be made too small
    val root = scene.getRoot.asInstanceOf[javafx.scene.layout.Region]

    primaryStage.setMinWidth(root.minWidth(-1))
    primaryStage.setMinHeight(root.minHeight(-1))
  }

  // Helper to wrap content with the side video if enabled
  private def finalizeScene(content: javafx.scene.layout.Region): Scene = {
    // Enforce the standard Sudoku window size for the content part
    content.setMinWidth(600)
    content.setMinHeight(650)
    content.setPrefWidth(600)
    content.setPrefHeight(650)

    val root = new HBox()
    root.setAlignment(Pos.CENTER)
    root.getChildren.add(content)

    if (GameConfig.showVideo && videoMediaPlayer != null) {
      val mv = new MediaView(videoMediaPlayer)
      mv.setFitHeight(650) // Match the height of the Sudoku window
      mv.setPreserveRatio(true)
      root.getChildren.add(mv)
    }

    val scene = new Scene(root)
    scene.getStylesheets.add(getClass.getResource("/css/style.css").toExternalForm)
    if (GameConfig.pinkMode) {
      scene.getStylesheets.add(getClass.getResource("/css/pink.css").toExternalForm)
    }
    scene
  }

  private def createGameScene(board: Board): Scene = {
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
    numButtons.setAlignment(Pos.CENTER)

    // Making the sudoku grid
    for (_ <- 0 until board.N) {
      val col = new javafx.scene.layout.ColumnConstraints()
      col.setPercentWidth(100.0 / board.N)
      boardFX.getColumnConstraints.add(col)

      val row = new javafx.scene.layout.RowConstraints()
      row.setPercentHeight(100.0 / board.N)
      boardFX.getRowConstraints.add(row)
    }

    for (i <- 0 until board.N; j <- 0 until board.N) {
      val cell = new CellButton(
        CellPos(i, j),
        gameState.board.getSquare(i, j)
      )
      cell.setMaxSize(Double.MaxValue, Double.MaxValue)
      cell.setPrefSize(50, 50)
      cells = cells :+ cell

      cell.setFocusTraversable(false)
      // CSS look
      cell.getStyleClass.add("cell")
      if (!cell.isMutable) cell.getStyleClass.add("cell-given")

      // lines for small sections (3x3 boxes in 9x9 sudoku)
      setSudokuBorders(cell, i, j, board.boxHeight, board.boxWidth)

      cell.setOnAction(_ => handleCellSelection(cell))

      boardFX.add(cell, j, i)
    }

    // Bottom buttons - input, erase, undo, hints
    for (i <- 1 to 9) {
      val button = new Button(i.toString)
      button.getStyleClass.add("key")
      button.setFocusTraversable(false)
      button.setOnAction(_ => trySetValue(i))
      numButtons.add(button, i - 1, 0)
    }

    val eraseButton = new Button("X")
    eraseButton.getStyleClass.add("key")
    eraseButton.setFocusTraversable(false)
    eraseButton.setOnAction(_ => trySetValue(0))
    numButtons.add(eraseButton, 9, 0)

    val undoButton = new Button("Undo")
    undoButton.getStyleClass.addAll("key")
    undoButton.setFocusTraversable(false)
    undoButton.setOnAction(_ => undo())
    numButtons.add(undoButton, 10, 0)

    if (GameConfig.allowHints) {
      val hintButton = new Button("Hint")
      hintButton.getStyleClass.addAll("key", "hint")
      hintButton.setFocusTraversable(false)
      hintButton.setOnAction(_ => giveHint())
      numButtons.add(hintButton, 11, 0)
    }

    val topBar = new HBox(20)
    topBar.setAlignment(Pos.CENTER)

    // Top bar elements - back to menu, lives, timer
    val menuButton = new Button("Menu")
    menuButton.getStyleClass.add("key")
    menuButton.setFocusTraversable(false)
    menuButton.setOnAction(_ => confirmExitToMenu())

    // Makes menu button and timer & lives sligned to opposite sides
    val spacer = new Pane
    HBox.setHgrow(spacer, Priority.ALWAYS)
    topBar.getChildren.addAll(menuButton, spacer)

    livesLabel = new Label("")
    livesLabel.getStyleClass.add("timer")
    if (GameConfig.showLives) {
      topBar.getChildren.add(livesLabel)
    }
    updateLivesLabel()
    timerLabel = new Label("00:00")
    timerLabel.getStyleClass.add("timer")
    if (GameConfig.showTimer) {
      topBar.getChildren.add(timerLabel)
    }

    val root = new VBox(10, topBar, boardFX, numButtons)

    root.setAlignment(javafx.geometry.Pos.CENTER)
    root.setPadding(new Insets(20))

    val scene = finalizeScene(root)

    scene.addEventFilter(KeyEvent.KEY_PRESSED, e => handleGameKeyPress(e))
    startTimer()
    scene
  }

  private def confirmExitToMenu(): Unit = {
    val alert = new Alert(AlertType.CONFIRMATION)
    alert.setTitle("Exit to Menu")
    alert.setHeaderText("Are you sure that you want to exit to menu?")
    alert.setContentText("Your progress will not be saved.")

    val result = alert.showAndWait()
    if (result.isPresent && result.get() == ButtonType.OK) {
      stopTimer()
      changeScene(createMenuScene())
    }
  }

  private def createMenuScene(): Scene = {
    var selectedDifficulty = 3
    var selectedSize = 9

    val title = new Label("Sudoku")
    title.getStyleClass.add("menu-title")

    val subtitle = new Label("Choose difficulty & board size:")
    subtitle.getStyleClass.add("menu-subtitle")
    subtitle.setStyle("-fx-font-size: 14px;")

    val difficultyCombo = new ComboBox[String]()
    difficultyCombo.getItems.addAll("Very Easy", "Easy", "Medium", "Hard", "Very Hard")
    difficultyCombo.setValue("Medium")
    difficultyCombo.getStyleClass.addAll("key","menu-button")
    difficultyCombo.setPrefWidth(200)
    difficultyCombo.setOnAction(_ => selectedDifficulty = difficultyCombo.getValue match {
      case "Very Easy" => 1
      case "Easy"      => 2
      case "Medium"    => 3
      case "Hard"      => 4
      case "Very Hard" => 5
      case _           => 3
    })

    val sizeCombo = new ComboBox[String]()
    sizeCombo.getItems.addAll("9 x 9", "6 x 6", "4 x 4")
    sizeCombo.setValue("9 x 9")
    sizeCombo.getStyleClass.addAll("key", "menu-button")
    sizeCombo.setPrefWidth(200)
    sizeCombo.setOnAction(_ => selectedSize = sizeCombo.getValue match {
      case "9 x 9" => 9
      case "6 x 6" => 6
      case "4 x 4" => 4
      case _ => 9
    })

    val playBtn = new Button("Play")
    playBtn.getStyleClass.addAll("key", "menu-button")
    playBtn.setPrefWidth(200)
    playBtn.setDefaultButton(true)
    playBtn.setOnAction(_ => {
      gameState = new GameState(selectedDifficulty, selectedSize)
      val scene = createGameScene(gameState.board)
      changeScene(scene)
      // This forces the window to respect the content's minimum size
      primaryStage.setMinWidth(scene.getRoot.minWidth(-1))
      primaryStage.setMinHeight(scene.getRoot.minHeight(-1))
    })

    val rulesBtn = new Button("Rules")
    rulesBtn.getStyleClass.addAll("key", "menu-button")
    rulesBtn.setPrefWidth(200)
    rulesBtn.setOnAction(_ => {

      changeScene(createRulesScene())
    })

    val controlsBtn = new Button("Controls")
    controlsBtn.getStyleClass.addAll("key", "menu-button")
    controlsBtn.setPrefWidth(200)
    controlsBtn.setOnAction(_ => changeScene(createControlsScene()))

    val settingsBtn = new Button("Settings")
    settingsBtn.getStyleClass.addAll("key", "menu-button")
    settingsBtn.setPrefWidth(200)
    settingsBtn.setOnAction(_ => changeScene(createSettingsScene()))

    val buttons = new VBox(12, playBtn, rulesBtn, controlsBtn, settingsBtn)
    buttons.setAlignment(Pos.CENTER)

    val difficultyBox = new VBox(5, subtitle, difficultyCombo, sizeCombo)
    difficultyBox.setAlignment(Pos.CENTER)

    val root = new VBox(25, title, difficultyBox, buttons)
    root.setPadding(new Insets(30))
    root.setAlignment(Pos.CENTER)

    root.setMinSize(600, 650)

    val scene = finalizeScene(root)
    Platform.runLater(() => playBtn.requestFocus())
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
    backBtn.setOnAction(_ => changeScene(createMenuScene()))

    val root = new VBox(18, title, rulesText, backBtn)
    root.setPadding(new Insets(30))
    root.setAlignment(Pos.CENTER)

    return finalizeScene(root)
  }

  private def createControlsScene(): Scene = {
    val title = new Label("Controls")
    title.getStyleClass.add("menu-title")

    val content = new Label(
      "• Mouse: Click cells to select, click keypad to enter.\n" +
        "• Arrows: Move selection.\n" +
        "• 1-9: Enter number.\n" +
        "• 0 / Backspace / Delete: Clear cell.\n" +
        "• Ctrl + Z: Undo.\n" +
        "• H: Hint.\n" +
        "• M: Menu."
    )
    content.getStyleClass.add("rules-text")
    content.setWrapText(true)

    val backBtn = new Button("Back")
    backBtn.getStyleClass.addAll("key", "menu-button")
    backBtn.setPrefWidth(200)
    backBtn.setOnAction(_ => changeScene(createMenuScene()))

    val root = new VBox(18, title, content, backBtn)
    root.setPadding(new Insets(30))
    root.setAlignment(Pos.CENTER)

    return finalizeScene(root)
  }

  private def createSettingsScene(): Scene = {
    val title = new Label("Settings")
    title.getStyleClass.add("menu-title")

    def createCheckbox(text: String, isSelected: Boolean, onToggle: Boolean => Unit): CheckBox = {
      val cb = new CheckBox(text)
      cb.setSelected(isSelected)
      cb.getStyleClass.add("menu-radio")
      cb.setOnAction(_ => onToggle(cb.isSelected))
      cb
    }

    val hintsCb = createCheckbox("Allow Hints", GameConfig.allowHints, v => GameConfig.allowHints = v)
    val timerCb = createCheckbox("Allow Timer", GameConfig.showTimer, v => GameConfig.showTimer = v)
    val livesCb = createCheckbox("Enable Lives", GameConfig.showLives, v => GameConfig.showLives = v)
    val musicCb = createCheckbox("Enable Epic Music", GameConfig.musicEnabled, v => {
      GameConfig.musicEnabled = v
      if (mediaPlayer != null) {
        if (v) mediaPlayer.play() else mediaPlayer.pause()
      }
    })
    val pinkCb = createCheckbox("Enable Pink Mode", GameConfig.pinkMode, v => {
      GameConfig.pinkMode = v
      val scene = primaryStage.getScene
      if (v) {
        if (!scene.getStylesheets.contains(getClass.getResource("/css/pink.css").toExternalForm))
          scene.getStylesheets.add(getClass.getResource("/css/pink.css").toExternalForm)
      } else {
        scene.getStylesheets.remove(getClass.getResource("/css/pink.css").toExternalForm)
      }
    })
    val videoCb = createCheckbox("Enable Focus Mode", GameConfig.showVideo, v => {
      GameConfig.showVideo = v
      if (videoMediaPlayer != null) {
        if (v) videoMediaPlayer.play() else videoMediaPlayer.pause()
      }
      // Refresh the scene to update layout (add/remove video pane)
      changeScene(createSettingsScene())
    })

    val backBtn = new Button("Back")
    backBtn.getStyleClass.addAll("key", "menu-button")
    backBtn.setPrefWidth(200)
    backBtn.setOnAction(_ => changeScene(createMenuScene()))

    val root = new VBox(20, title, hintsCb, timerCb, livesCb, pinkCb, musicCb, videoCb, backBtn)
    root.setPadding(new Insets(30))
    root.setAlignment(Pos.CENTER)

    return finalizeScene(root)
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
      gameState = new GameState(selectedDifficulty, gameState.board.N)
      changeScene(createGameScene(gameState.board))
    })

    val menuBtn = new Button("Menu")
    menuBtn.getStyleClass.addAll("key", "menu-button")
    menuBtn.setPrefWidth(200)
    menuBtn.setOnAction(_ => changeScene(createMenuScene()))

    val exitBtn = new Button("Exit")
    exitBtn.getStyleClass.addAll("key", "menu-button")
    exitBtn.setPrefWidth(200)
    exitBtn.setOnAction(_ => Platform.exit())

    val buttons = new HBox(12, playAgainBtn, menuBtn, exitBtn)
    buttons.setAlignment(Pos.CENTER)

    val root = new VBox(18, title, timeLbl, buttons)
    root.setPadding(new Insets(30))
    root.setAlignment(Pos.CENTER)

    return finalizeScene(root)
  }

  override def start(stage: Stage): Unit = {
    primaryStage = stage
    stage.setTitle("Sudoku")
    initMusic()
    initVideo()

    val scene = createMenuScene()
    stage.setScene(scene)

    // sets minimum size so the window cannot be made too small
    val root = scene.getRoot.asInstanceOf[javafx.scene.layout.Region]

    primaryStage.setMinWidth(root.minWidth(-1))
    primaryStage.setMinHeight(root.minHeight(-1))

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
      if (GameConfig.showLives) {
        val remaining = gameState.maxLives - gameState.mistakes
        livesLabel.setText(s"Lives: $remaining/${gameState.maxLives}")
      } else {
        livesLabel.setText("")
      }
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
    menuBtn.setOnAction(_ => changeScene(createMenuScene()))

    val exitBtn = new Button("Exit")
    exitBtn.getStyleClass.addAll("key", "menu-button")
    exitBtn.setPrefWidth(200)
    exitBtn.setOnAction(_ => Platform.exit())

    val root = new VBox(20, title, msg, menuBtn, exitBtn)
    root.setPadding(new Insets(30))
    root.setAlignment(Pos.CENTER)

    // Reusing existing CSS
    return finalizeScene(root)
  }

}