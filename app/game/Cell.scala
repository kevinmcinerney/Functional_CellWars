package game

import breeze.linalg.{max, min}


/**
  * Created by kevin on 29/04/17.
  */
case class Cell(topLeft: Coordinate, botRight: Coordinate) {

  def up: Cell = {
    Cell(
      Coordinate(topLeft.x + 0, topLeft.y - 1)
      , Coordinate(botRight.x + 0, botRight.y - 1))
  }

  def down: Cell = {
    Cell(
      Coordinate(topLeft.x + 0, topLeft.y + 1)
      , Coordinate(botRight.x + 0, botRight.y + 1))
  }

  def left: Cell = {
    Cell(
      Coordinate(topLeft.x - 1, topLeft.y + 0)
      , Coordinate(botRight.x - 0, botRight.y + 0))
  }

  def right: Cell = {
    Cell(
      Coordinate(topLeft.x + 1, topLeft.y + 0)
      , Coordinate(botRight.x + 1, botRight.y + 0))
  }

  def topRight = Coordinate(botRight.x, topLeft.y)

  def botLeft = Coordinate(topLeft.x, botRight.y)

  def innerCells: List[Coordinate] = {
    for {
      x <- topLeft.x + 1 until botRight.x
      y <- topLeft.y + 1 until botRight.y
    } yield Coordinate(x, y)
  }.toList

  def outerCells: List[Coordinate] = {
    for {
      x <- topLeft.x to botRight.x
      y <- topLeft.y to botRight.y
    } yield Coordinate(x, y)
  }.toList

  def contains(other: Cell): Boolean =
    outerCells.exists(point => other.innerCells.contains(point))

  def merge(other: Cell): Option[Cell] = {
    if(this contains other){
      val topLeftX = min(topLeft.x, other.topLeft.x)
      val topLeftY = min(topLeft.y, other.topLeft.y)
      val botRightX = max(botRight.x, other.botRight.x)
      val botRightY = max(botRight.y, other.botRight.y)
      Some(
        Cell(
          Coordinate(topLeftX, topLeftY),
          Coordinate(botRightX, botRightY)
        )
      )
    } else None
  }

  override def toString: String = " ("+ topLeft.x + "," + topLeft.y + ")-(" + botRight.x + "," + botRight.y + ") "

}