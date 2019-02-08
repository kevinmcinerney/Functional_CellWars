package game

import breeze.linalg.{max, min}
import breeze.math.MutablizingAdaptor.CoordinateFieldAdaptor


/**
  * Created by kevin on 29/04/17.
  */
case class Cell(topLeft: Point, botRight: Point){

  def up: Cell = {
    Cell(
      Point(topLeft.x + 0, topLeft.y - 1)
      , Point(botRight.x + 0, botRight.y - 1))
}
  
  def down: Cell = {
    Cell(
      Point(topLeft.x + 0, topLeft.y + 1)
      , Point(botRight.x + 0, botRight.y + 1))
  }

  def left: Cell = {
    Cell(
      Point(topLeft.x - 1, topLeft.y + 0)
      , Point(botRight.x - 1, botRight.y + 0))
  }

  def right: Cell = {
    Cell(
      Point(topLeft.x + 1, topLeft.y + 0)
      , Point(botRight.x + 1, botRight.y + 0))
  }

  def realCell: Boolean = {
    (botRight.x - topLeft.x) == 3 && (botRight.y - topLeft.y) == 3
  }

  def nucleus: Option[Point] = {
    if(realCell){
      Some(Point(topLeft.x + 1, topLeft.y + 1 ))
    }
    else{ None }
  }

  def innerCells: List[Point] = {
    for {
      x <- topLeft.x + 1 until botRight.x
      y <- topLeft.y + 1 until botRight.y
    } yield Point(x, y)
  }.toList

  def outerCells: List[Point] = {
    for {
      x <- topLeft.x to botRight.x
      y <- topLeft.y to botRight.y
    } yield Point(x, y)
  }.toList

  def contains(other: Cell): Boolean =
    outerCells.exists(point => other.innerCells.contains(point))

  def contains(other: Point): Boolean =
    outerCells.contains(other)

  def fullyInsideOf(other: Cell): Boolean = {
    outerCells.forall(p => other.contains(p))
  }

  def merge(other: Cell): Cell = {

    val topLeftX = min(topLeft.x, other.topLeft.x)
    val topLeftY = min(topLeft.y, other.topLeft.y)
    val botRightX = max(botRight.x, other.botRight.x)
    val botRightY = max(botRight.y, other.botRight.y)
    Cell(topLeftX, topLeftY,botRightX, botRightY)
  }

  def area: Int = (botRight.x - topLeft.x) * (botRight.y - topLeft.y)

  def compare(other: Cell): Cell = {
    if(this.area > other.area) this
    else if (other.area > this.area) other
    else this
  }

  override def toString: String = " ("+ topLeft.x + "," + topLeft.y + ")-(" + botRight.x + "," + botRight.y + ") "

}

object Cell{

  def apply(tlx: Int, tly: Int, brx: Int, bry: Int): Cell =
    Cell(Point(tlx,tly),Point(brx,bry))


  import play.api.libs.json._

  implicit val cellFormats = Json.format[Cell]

  def writeCell(cell: Cell): JsValue =
    Json.toJson(cell)


  def readCell(jsonCell: JsValue): Cell = {
    val topLeft = (jsonCell \ "topLeft").as[Point]
    val botRight = (jsonCell \ "botRight").as[Point]
    Cell(topLeft, botRight)
  }


}