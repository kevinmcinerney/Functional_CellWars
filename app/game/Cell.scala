package game

import breeze.linalg.{max, min}


/**
  * Created by kevin on 29/04/17.
  */
case class Cell(topLeft: Coordinate, botRight: Coordinate) {


  def up = Cell.up(this)

  def down = Cell.down(this)

  def left = Cell.left(this)

  def right = Cell.right(this)

  def realCell = Cell.realCell(this)

  def nucleus = Cell.nucleus(this)

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

  def contains(other: Coordinate): Boolean =
    outerCells.contains(other)


  def merge(other: Cell): Cell = {

    val topLeftX = min(topLeft.x, other.topLeft.x)
    val topLeftY = min(topLeft.y, other.topLeft.y)
    val botRightX = max(botRight.x, other.botRight.x)
    val botRightY = max(botRight.y, other.botRight.y)
    Cell(
      Coordinate(topLeftX, topLeftY),
      Coordinate(botRightX, botRightY)
    )
  }

  override def toString: String = " ("+ topLeft.x + "," + topLeft.y + ")-(" + botRight.x + "," + botRight.y + ") "

}

object Cell{

  def up(cell: Cell): Cell = {
    Cell(
      Coordinate(cell.topLeft.x + 0, cell.topLeft.y - 1)
      , Coordinate(cell.botRight.x + 0, cell.botRight.y - 1))
  }

  def down(cell: Cell): Cell = {
    Cell(
      Coordinate(cell.topLeft.x + 0, cell.topLeft.y + 1)
      , Coordinate(cell.botRight.x + 0, cell.botRight.y + 1))
  }

  def left(cell: Cell): Cell = {
    Cell(
      Coordinate(cell.topLeft.x - 1, cell.topLeft.y + 0)
      , Coordinate(cell.botRight.x - 1, cell.botRight.y + 0))
  }

  def right(cell: Cell): Cell = {
    Cell(
      Coordinate(cell.topLeft.x + 1, cell.topLeft.y + 0)
      , Coordinate(cell.botRight.x + 1, cell.botRight.y + 0))
  }

  def realCell(cell: Cell): Boolean = {
    (cell.botRight.x - cell.topLeft.x) == 3 && (cell.botRight.y - cell.topLeft.y) == 3
  }

  def nucleus(cell: Cell): Option[Coordinate] = {
    if(realCell(cell)){
      Some(Coordinate(cell.topLeft.x + 1, cell.topLeft.y + 1 ))
    }
    else{ None }
  }

  //  def drawCells: List[Coordinate] = {
  //    for {
  //      x <- topLeft.x until botRight.x
  //      y <- topLeft.y until botRight.y
  //    } yield Coordinate(x, y)
  //  }.toList

  //  def centerCell: Coordinate = {
  //    Coordinate(topLeft.x + 1, topLeft.y + 1)
  //  }


  import play.api.libs.json._

  implicit val cellFormats = Json.format[Cell]

  def writeCell(cell: Cell): JsValue = {
    Json.toJson(cell)
  }

  def readCell(jsonCell: JsValue): Cell = {
    val topLeft = (jsonCell \ "topLeft").as[Coordinate]
    val botRight = (jsonCell \ "botRight").as[Coordinate]
    Cell(topLeft, botRight)
  }


}