package game
import com.google.gson.Gson

/**
  * Created by kevin on 29/04/17.
  */
trait BoardTrait {

  var board: Array[Array[Int]]

  def onBoard(i: Int): Boolean

  def isValidMove(cell: Cell): Boolean

}

case class Board(dimensions: Int) extends BoardTrait {

  override var board = Array.ofDim[Int](dimensions,dimensions)

  def onBoard(i: Int): Boolean = i >= 0 && i < dimensions

  def isValidMove(cell: Cell): Boolean = {
    onBoard(cell.topLeft.x) && onBoard(cell.botRight.x) &&
      onBoard(cell.topLeft.y) && onBoard(cell.botRight.y)
  }

  def boardToJSON(): String = {
    // Read in teams
    // Make Array of Board
    // JSONify for UI
    ???
  }

}