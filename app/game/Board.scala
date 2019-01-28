package game


import com.google.gson.Gson

/**
  * Created by kevin on 29/04/17.
  */
trait BoardTrait {

  def onBoard(i: Int): Boolean

  def isValidMove(cell: Cell): Boolean

}

case class Grid(height: Int, width: Int, team: Int, x: Int, y: Int)

case class Board(dimensions: Int) extends BoardTrait {

  def onBoard(i: Int): Boolean = i >= 0 && i < dimensions

  def isValidMove(cell: Cell): Boolean = {
    onBoard(cell.topLeft.x) && onBoard(cell.botRight.x) &&
      onBoard(cell.topLeft.y) && onBoard(cell.botRight.y)
  }

  def boardToJSON(): String = {

    // Constructor here for two teams
    val board: Array[Array[Grid]] = makeBoard()
    val gson = new Gson
    val jsonString = gson.toJson(board)
    jsonString
  }

  private def makeBoard(): Array[Array[Grid]] = {

    val board = Array.ofDim[Grid](dimensions, dimensions)
    val length = 600 / dimensions
    for (x <- 0 until 600 by length) {
      for ( y <- 0 until 600 by length) {
        board(x/length)(y/length) = Grid(length,length, 1, x, y)
      }
    }

    board

  }

}