 import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.html.Canvas
import scala.scalajs.js
import scala.util.Random

// Define Cell Class and Types
sealed trait CellType
case object SnakeBody extends CellType
case object Apple extends CellType
case object Empty extends CellType

case class Cell(position: (Int, Int), cellType: CellType)

object NewSnakeGameLogic {
  // Constants
  val SCREEN_WIDTH = 800
  val SCREEN_HEIGHT = 600
  val SNAKE_SIZE = 20
  val SNAKE_COLOR = "rgb(0, 255, 0)"
  val BG_COLOR = "rgb(0, 4, 0)"
  var gameOver = false
  var isReverseMode = false
  var previousStates: List[List[(Int, Int)]] = List()

  def toggleReverseMode(): Unit = {
    if (!isReverseMode) {
      // Enable reverse mode
      isReverseMode = true
      previousStates = List(snakeSegments.map(identity)) // Save the current state
    } else {
      // Disable reverse mode
      isReverseMode = false
      if (previousStates.nonEmpty) {
        // Restore the previous state
        snakeSegments = previousStates.head
        previousStates = previousStates.tail
      }
    }
  }

  // Draw the snake head with an indicator arrow
  def drawSnakeHead(ctx: dom.CanvasRenderingContext2D): Unit = {
    val (headX, headY) = snakeSegments.head
    ctx.fillStyle = SNAKE_COLOR
    ctx.fillRect(headX, headY, SNAKE_SIZE, SNAKE_SIZE)

    // Draw the indicator arrow based on the snake's current direction
    ctx.fillStyle = "yellow"
    ctx.beginPath()
    ctx.moveTo(headX + SNAKE_SIZE / 2, headY + SNAKE_SIZE / 2)

    snakeDirection match {
      case "up" => ctx.lineTo(headX + SNAKE_SIZE / 2, headY)
      case "down" => ctx.lineTo(headX + SNAKE_SIZE / 2, headY + SNAKE_SIZE)
      case "left" => ctx.lineTo(headX, headY + SNAKE_SIZE / 2)
      case "right" => ctx.lineTo(headX + SNAKE_SIZE, headY + SNAKE_SIZE / 2)
    }

    ctx.closePath()
    ctx.fill()
  }

  // Initialize Snake
  var snakeSegments: List[(Int, Int)] = List((5 * SNAKE_SIZE, 3 * SNAKE_SIZE))
  var snakeDirection = "right"
  var snakeGrowth = 0

  // Function to generate a random position for the apple
  def generateApplePosition(): (Int, Int) = {
    var x, y = 0
    do {
      x = Random.nextInt(SCREEN_WIDTH / SNAKE_SIZE) * SNAKE_SIZE
      y = Random.nextInt(SCREEN_HEIGHT / SNAKE_SIZE) * SNAKE_SIZE
    } while (snakeSegments.contains((x, y))) // Ensure the apple doesn't overlap with the snake
    (x, y)
  }

  // Function to check for collisions
  def checkCollisions(): Boolean = {
    val head = snakeSegments.head
    // Check for collisions with boundaries
    if (head._1 < 0 || head._1 >= SCREEN_WIDTH || head._2 < 0 || head._2 >= SCREEN_HEIGHT) {
      return true
    }
    // Check for self-collision
    if (snakeSegments.tail.contains(head)) {
      return true
    }
    false
  }

  // Function to update the game state
  def updateGameState(): Unit = {
    // Move the Snake
    moveSnake()

    // Check if the snake hits the screen boundaries
    if (snakeSegments.head._1 < 0) {
      snakeSegments = snakeSegments.updated(0, (SCREEN_WIDTH - SNAKE_SIZE, snakeSegments.head._2))
    } else if (snakeSegments.head._1 >= SCREEN_WIDTH) {
      snakeSegments = snakeSegments.updated(0, (0, snakeSegments.head._2))
    }
    if (snakeSegments.head._2 < 0) {
      snakeSegments = snakeSegments.updated(0, (snakeSegments.head._1, SCREEN_HEIGHT - SNAKE_SIZE))
    } else if (snakeSegments.head._2 >= SCREEN_HEIGHT) {
      snakeSegments = snakeSegments.updated(0, (snakeSegments.head._1, 0))
    }

    // Keep the snake length equal to or less than segments
    while (snakeSegments.size > snakeSegments.size * 3) {
      snakeSegments = snakeSegments.dropRight(1)
    }

    // Add segments to the snake's body if the snake has grown
    while (snakeGrowth > 0) {
      snakeSegments = snakeSegments :+ snakeSegments.last
      snakeGrowth -= 1
    }
  }

  // Function to reset the game (for game over)
  def resetGame(): Unit = {
    snakeSegments = List((5 * SNAKE_SIZE, 3 * SNAKE_SIZE))
    snakeDirection = "right"
    snakeGrowth = 0
    applePosition = generateApplePosition()
    gameOver = false
  }

  // Function to change the snake's direction
  def changeDirection(newDirection: String): Unit = {
    // Ensure that the snake cannot reverse its direction instantly
    if ((newDirection == "up" && snakeDirection != "down") ||
      (newDirection == "down" && snakeDirection != "up") ||
      (newDirection == "left" && snakeDirection != "right") ||
      (newDirection == "right" && snakeDirection != "left")) {
      snakeDirection = newDirection
    }
  }

  // Initialize cells
  val canvasWidth = 800
  val canvasHeight = 600
  val cellSize = 20

  val cells: List[Cell] = {
    val positions = for {
      x <- 0 until canvasWidth by cellSize
      y <- 0 until canvasHeight by cellSize
    } yield (x, y)

    positions.map(position => Cell(position, Empty)).toList
  }

  // Function to create an ASCII representation of the game board
  def createAsciiBoard(snakeSegments: List[(Int, Int)], applePosition: (Int, Int)): String = {
    val columns = canvasWidth / cellSize
    val rows = canvasHeight / cellSize
    val board = Array.ofDim[Char](rows, columns)

    // Initialize the board with empty cells
    for (row <- 0 until rows) {
      for (col <- 0 until columns) {
        board(row)(col) = '.'
      }
    }

    // Place the snake on the board
    for ((x, y) <- snakeSegments) {
      board(y / cellSize)(x / cellSize) = 'O'
    }

    // Place the apple on the board
    val (appleX, appleY) = applePosition
    board(appleY / cellSize)(appleX / cellSize) = 'A'

    // Create the ASCII representation
    val asciiBoard = new StringBuilder()
    for (row <- 0 until rows) {
      for (col <- 0 until columns) {
        asciiBoard.append(board(row)(col))
      }
      asciiBoard.append('\n')
    }

    asciiBoard.toString()
  }
}

object NewSnakeGame {
  def main(args: Array[String]): Unit = {
    val canvas = dom.document.getElementById("gameCanvas").asInstanceOf[Canvas]
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    // Function to display "Game Over" message and restart button
    def showGameOverScreen(): Unit = {
      ctx.fillStyle = "white"
      ctx.font = "48px sans-serif"
      ctx.textAlign = "center"
      ctx.fillText("Game Over", NewSnakeGameLogic.SCREEN_WIDTH / 2, NewSnakeGameLogic.SCREEN_HEIGHT / 2 - 48)

      // Create a restart button
      val restartButton = dom.document.createElement("button").asInstanceOf[dom.html.Button]
      restartButton.textContent = "Restart Game"
      restartButton.style.fontSize = "24px"
      restartButton.style.position = "absolute"
      restartButton.style.left = s"${NewSnakeGameLogic.SCREEN_WIDTH / 2 - 100}px"
      restartButton.style.top = s"${NewSnakeGameLogic.SCREEN_HEIGHT / 2}px"

      // Add a click event listener to restart the game when the button is clicked
      restartButton.addEventListener("click", (_: dom.MouseEvent) => {
        NewSnakeGameLogic.resetGame()
        restartButton.parentNode.removeChild(restartButton)
        // Start the game loop again
        gameLoop()
      })

      // Append the button to the body
      dom.document.body.appendChild(restartButton)
    }

    // Handle keyboard events
    dom.window.addEventListener("keydown", (e: dom.KeyboardEvent) => {
      e.keyCode match {
        case KeyCode.Up => NewSnakeGameLogic.changeDirection("up")
        case KeyCode.Down => NewSnakeGameLogic.changeDirection("down")
        case KeyCode.Left => NewSnakeGameLogic.changeDirection("left")
        case KeyCode.Right => NewSnakeGameLogic.changeDirection("right")
        case KeyCode.R if !NewSnakeGameLogic.gameOver => NewSnakeGameLogic.toggleReverseMode() // Enable reverse mode with the "r" key
        case _ =>
      }
    })

    // Game loop
    def gameLoop(): Unit = {
      if (!NewSnakeGameLogic.isReverseMode) {
        // Only update the game state when not in reverse mode
        NewSnakeGameLogic.updateGameState()

        // Check for collisions
        if (NewSnakeGameLogic.checkCollisions()) {
          // Handle game over
          dom.window.alert("Game Over")
          NewSnakeGameLogic.resetGame()
          return
        }

        // Check if the snake eats the apple
        if (snake.head == apple) {
          // Increase snake size and generate a new apple
          snake = snake :+ snake.last
          apple = generateApplePosition()
        }
      }

      // Clear the canvas
      ctx.fillStyle = NewSnakeGameLogic.BG_COLOR
      ctx.fillRect(0, 0, NewSnakeGameLogic.SCREEN_WIDTH, NewSnakeGameLogic.SCREEN_HEIGHT)

      // Draw the game based on cells
      for (cell <- NewSnakeGameLogic.cells) {
        ctx.fillStyle = cell.cellType match {
          case SnakeBody => NewSnakeGameLogic.SNAKE_COLOR
          case Apple => "red"
          case Empty => NewSnakeGameLogic.BG_COLOR
        }
        ctx.fillRect(cell.position._1, cell.position._2, NewSnakeGameLogic.SNAKE_SIZE, NewSnakeGameLogic.SNAKE_SIZE)
      }

      // Update the game board cells
      cells = Array.tabulate(numRows, numCols) { (row, col) =>
        if (snake.contains((col, row))) Cell(col, row, SnakeBody)
        else if (apple == (col, row)) Cell(col, row, Apple)
        else Cell(col, row, Empty)
      }

      // Draw the snake head and indicator arrow
      NewSnakeGameLogic.drawSnakeHead(ctx)

      // Check if the game is over
      if (NewSnakeGameLogic.gameOver) {
        showGameOverScreen()
        return // Stop the game loop
      }

      // Call the game loop recursively
      dom.window.requestAnimationFrame(_ => gameLoop())
    }

    // Start the game loop
    gameLoop()
  }
}
