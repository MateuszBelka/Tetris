package tetris.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import tetris.game._
import tetris.logic.TetrisLogic._
import util.control.Breaks._

class PlayerPosAndMatrix(var matrix: Array[Array[TetrisBlock]],
                         var y: Int,
                         var x: Int) {
  def isMatrixEmptyAt(j: Int, i: Int): Boolean = matrix(j)(i) == Empty
} 

class TetrisLogic(val randomGen: RandomGenerator,
                  val nrColumns: Int,
                  val nrRows: Int,
                  val initialBoard: Seq[Seq[TetrisBlock]]) {

  def this(random: RandomGenerator, nrColumns: Int, nrRows: Int) =
    this(random, nrColumns, nrRows, makeEmptyBoard(nrColumns, nrRows))

  def this() =
    this(new ScalaRandomGen(), DefaultWidth, DefaultHeight, makeEmptyBoard(DefaultWidth, DefaultHeight))

  val nrTetrominoes: Int = 7
  var player: PlayerPosAndMatrix = new PlayerPosAndMatrix(returnNewPiece(), 0, 0)
  player.x = selectInitialOffsetX()
  var board: Array[Array[TetrisBlock]] = Array.ofDim[TetrisBlock](nrRows, nrColumns)
  populateInitialBoard()

  def populateInitialBoard(): Unit = {
    for (y <- 0 until nrRows; x <- 0 until nrColumns) {
      board(y)(x) = initialBoard(y)(x)
    }
  }

  def returnNewPiece(): Array[Array[TetrisBlock]] = {
    val random = randomGen.randomInt(nrTetrominoes)

    random match {
      case 0 => Array(
        Array(Empty, Empty, Empty, Empty),
        Array(IBlock, IBlock, IBlock, IBlock),
        Array(Empty, Empty, Empty, Empty),
        Array(Empty, Empty, Empty, Empty)
      )
      case 1 => Array(
        Array(JBlock, Empty, Empty),
        Array(JBlock, JBlock, JBlock),
        Array(Empty, Empty, Empty)
      )
      case 2 => Array(
        Array(Empty, Empty, LBlock),
        Array(LBlock, LBlock, LBlock),
        Array(Empty, Empty, Empty)
      )
      case 3 => Array(
        Array(OBlock, OBlock),
        Array(OBlock, OBlock)
      )
      case 4 => Array(
        Array(Empty, SBlock, SBlock),
        Array(SBlock, SBlock, Empty),
        Array(Empty, Empty, Empty),
      )
      case 5 => Array(
        Array(Empty, TBlock, Empty),
        Array(TBlock, TBlock, TBlock),
        Array(Empty, Empty, Empty),
      )
      case 6 => Array(
        Array(ZBlock, ZBlock, Empty),
        Array(Empty, ZBlock, ZBlock),
        Array(Empty, Empty, Empty)
      )
    }
  }

  def selectInitialOffsetX(): Int = {
    /*
     * Due to OBlock being the only 2x2 tetromino
     * a different offset has to be chosen to position in the middle
     */
    var offset: Int = 0
    if (player.matrix.length != 2) offset = 1

    if (nrColumns % 2 == 0) (nrColumns / 2) - (1 + offset)
    else Math.floor(nrColumns / 2).toInt - offset
  }

  def merge(): Unit = {
    for (y <- player.matrix.indices; x <- player.matrix(y).indices) {
      if (!player.isMatrixEmptyAt(y,x)) {
        board(y + player.y)(x + player.x) = player.matrix(y)(x)
      }
    }
  }

  def isPlayerOverlappingAnything: Boolean = isPlayerOverlappingEdgesX || isPlayerOverlappingEdgesY || isPlayerOverlappingBoard

  def isPlayerOverlappingEdgesY: Boolean = {
    if (player.y <= board.length - player.matrix.length) return false
    for (y <- board.length - player.y until player.matrix.length; x <- player.matrix(y).indices) {
      if (!player.isMatrixEmptyAt(y,x)) return true
    }
    false
  }

  def isPlayerOverlappingEdgesX: Boolean = {
    if (player.x < 0) {
      isPlayerOverlappingEdgesXlhs
    } else if (player.x > board.head.length - player.matrix.head.length) {
      isPlayerOverlappingEdgesXrhs
    } else {
      false
    }
  }

  def isPlayerOverlappingEdgesXlhs: Boolean ={
    for (y <- player.matrix.indices; x <- 0 until -player.x) {
      if (!player.isMatrixEmptyAt(y,x)) return true
    }
    false
  }

  def isPlayerOverlappingEdgesXrhs: Boolean ={
    for (y <- player.matrix.indices; x <- board.head.length - player.x until player.matrix(y).length) {
      if (!player.isMatrixEmptyAt(y,x)) return true
    }
    false
  }

  def isPlayerOverlappingBoard: Boolean = {
    for (y <- player.matrix.indices; x <- player.matrix.head.indices) {
      if (!player.isMatrixEmptyAt(y,x) && board(y + player.y)(x + player.x) != Empty) return true
    }
    false
  }

  def playerMoveXAxis(dir: Int): Unit = {
    if (isGameOver) return
    player.x += dir
    if (isPlayerOverlappingAnything) {
      player.x -= dir
    }
  }

  def playerRotate(dir: Int): Unit = {
    if (isGameOver) return
    val matrixCopy = player.matrix

    /*
     * Rotation can be achieved by firstly transposing the matrix and then
     * depending on the direction of rotation rotating it
     * with respect to either x or y axis
     */
    player.matrix = player.matrix.transpose
    if (dir > 0) playerMatrixReverseYAxis()
    else playerMatrixReverseXAxis()

    if (isPlayerOverlappingAnything) player.matrix = matrixCopy
  }

  def playerMatrixReverseYAxis(): Unit = {
    for (j <- player.matrix.indices; i <- 0 until (player.matrix(j).length / 2)) {
      val temp = player.matrix(j)(i)
      player.matrix(j)(i) = player.matrix(j)(player.matrix(j).length - i - 1)
      player.matrix(j)(player.matrix(j).length - i - 1) = temp
    }
  }

  def playerMatrixReverseXAxis(): Unit = {
    player.matrix = player.matrix.reverse
  }

  def boardSweep(): Unit = {
    var y: Int = board.length - 1
    while (y > 0) {
      if (!isBoardEmptyAtRow(y)) {
        moveFullRowToTop(y)
        emptyTopRowOfBoard(y)
        y += 1
      }
      y -= 1
    }
  }

  def isBoardEmptyAtRow(y: Int): Boolean = {
    for (x <- board(y).indices) {
      if (board(y)(x) == Empty) {
        return true
      }
    }
    false
  }

  def moveFullRowToTop(y: Int): Unit = {
    for (i <- y until 0 by -1) {
      val tmp = board(i)
      board(i) = board(i - 1)
      board(i - 1) = tmp
    }
  }

  def emptyTopRowOfBoard(y: Int): Unit = {
    for (i <- board.head.indices) {
      board.head(i) = Empty
    }
  }

  def playerHitBottomReset(): Unit = {
    player.y -= 1
    merge()
    playerNewPieceAndLocation()
    boardSweep()
  }

  def playerNewPieceAndLocation(): Unit = {
    player.matrix = returnNewPiece()
    player.y = 0
    player.x = selectInitialOffsetX()
  }

  def areCoordsOutsidePlayerPos(y: Int, x: Int): Boolean = y >= player.y && x >= player.x
  def areCoordsSmallerThanPlayerTotalLength(y: Int, x: Int): Boolean = y < player.matrix.length + player.y && x < player.matrix(0).length + player.x

  /*
   * System defined functions
   */
  def rotateLeft(): Unit = playerRotate(-1)

  def rotateRight(): Unit = playerRotate(1)

  def moveLeft(): Unit = playerMoveXAxis(-1)

  def moveRight(): Unit = playerMoveXAxis(1)

  def moveDown(): Unit = {
    if (isGameOver) return
    player.y += 1
    if (isPlayerOverlappingAnything) {
      playerHitBottomReset()
    }
  }

  def doHardDrop(): Unit = {
    if (isGameOver) return
    while (!isPlayerOverlappingAnything) {
      player.y += 1
    }
    playerHitBottomReset()
  }

  def isGameOver: Boolean = player.y == 0 && isPlayerOverlappingBoard

  def getBlockAt(x: Int, y: Int): TetrisBlock = {
    if (areCoordsOutsidePlayerPos(y,x) && areCoordsSmallerThanPlayerTotalLength(y,x) && !player.isMatrixEmptyAt(y - player.y, x - player.x)) {
      player.matrix(y - player.y)(x - player.x)
    } else if (board(y)(x) != Empty) {
      board(y)(x)
    } else {
      Empty
    }
  }
}

object TetrisLogic {

  def makeEmptyBoard(nrColumns: Int, nrRows: Int): Seq[Seq[TetrisBlock]] = {
    val emptyLine = Seq.fill(nrColumns)(Empty)
    Seq.fill(nrRows)(emptyLine)
  }

  val DefaultWidth: Int = 10
  val NrTopInvisibleLines: Int = 4
  val DefaultVisibleHeight: Int = 20
  val DefaultHeight: Int = DefaultVisibleHeight + NrTopInvisibleLines


  def apply() = new TetrisLogic(new ScalaRandomGen(),
    DefaultWidth,
    DefaultHeight,
    makeEmptyBoard(DefaultWidth, DefaultHeight))

}