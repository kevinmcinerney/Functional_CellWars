package game

import breeze.math.CoordinateField

/**
  * Created by kevin on 25/04/17.
  */

object Game {

  case class Cell(topLeft: Coordinate, botRight: Coordinate)
  case class Team(mark: Char, cells: List[Cell])
  case class Coordinate(x: Int, y: Int)


  trait BoardTrait {

    var board: Array[Array[Int]]

    def up(cell: Cell): Cell

    def down(cell: Cell): Cell

    def left(cell: Cell): Cell

    def right(cell: Cell): Cell

    def onBoard(i: Int): Boolean

    def isValidMove(move: Cell => Cell, cell: Cell): Boolean

  }

  case class Board(dimensions: Int) extends BoardTrait {

    override var board = Array.ofDim[Int](dimensions,dimensions)

    def up(cell: Cell): Cell = {
      Cell(
       Coordinate(cell.topLeft.x + 0, cell.topLeft.y - 1)
      ,Coordinate(cell.botRight.x + 0, cell.botRight.y - 1))
    }

    def down(cell: Cell): Cell = {
      Cell(
       Coordinate(cell.topLeft.x + 0, cell.topLeft.y + 1)
      ,Coordinate(cell.botRight.x + 0, cell.botRight.y + 1))
  }

    def left(cell: Cell): Cell =  {
      Cell(
       Coordinate(cell.topLeft.x - 1, cell.topLeft.y + 0)
      ,Coordinate(cell.botRight.x - 0, cell.botRight.y + 0))
    }

    def right(cell: Cell): Cell = {
      Cell(
       Coordinate(cell.topLeft.x + 1, cell.topLeft.y + 0)
      ,Coordinate(cell.botRight.x + 1, cell.botRight.y + 0))
    }

    def onBoard(i: Int): Boolean = i >= 0 && i < dimensions

    def isValidMove(move: Cell => Cell, cell: Cell): Boolean = {
      val newCell = move(cell)
      onBoard(newCell.topLeft.x) && onBoard(newCell.botRight.x) &&
      onBoard(newCell.topLeft.y) && onBoard(newCell.botRight.y)
    }

  }

}
