package game

import breeze.linalg.{max, min}
import breeze.numerics.abs



/**  Cell Trait **/
sealed trait Cell {

  def x1: Int
  def y1: Int
  def x2: Int
  def y2: Int

  def marker: Int = 0


  def up: RCell = {
    RCell(x1 + 0, y1 - 1, x2 + 0, y2 - 1, marker)
  }

  def down: RCell = {
    RCell(x1 + 0, y1 + 1, x2 + 0, y2 + 1, marker)
  }

  def left: RCell = {
    RCell(x1 - 1, y1 + 0, x2 - 1, y2 + 0, marker)
  }

  def right: RCell = {
    RCell(x1 + 1, y1 + 0, x2 + 1, y2 + 0, marker)
  }

  def innerPoints: List[Point] = {
    for {
      x <- x1 + 1 until x2
      y <- y1 + 1 until y2
    } yield Point(x, y)
  }.toList

  def allPoints: List[Point] = {
    for {
      x <- x1 to x2
      y <- y1 to y2
    } yield Point(x, y)
  }.toList

  def drawPoints: List[Point] = {
    for {
      x <- x1 until x2
      y <- y1 until y2
    } yield Point(x, y)
  }.toList

  def contains(other: Cell): Boolean =
    this != other &&
    allPoints.exists(point => other.innerPoints.contains(point))

  def contains(other: Point): Boolean = {
    allPoints.contains(other)
  }

  def area: Int = abs(x2 - x1) * abs(y2 - y1)

  def merge(other: Cell): Cell = {
    assert(this contains  other, "Shouldn't be merged")
    val topLeftX = min(x1, other.x1)
    val topLeftY = min(y1, other.y1)
    val botRightX = max(x2, other.x2)
    val botRightY = max(y2, other.y2)
    VCell(topLeftX, topLeftY,botRightX, botRightY, marker)
  }

  override def toString: String = " ("+ x1 + "," + y1 + ")-(" + x2 + "," + y2 + ") " + " Team: " + marker
}



/**  Vertex Trait **/
sealed trait Vertex { var visited = false }



/**  Real Cell **/
case class RCell(x1: Int, y1: Int,x2: Int, y2: Int, override val marker: Int) extends Cell with Vertex {

  require(abs(x1 - x2) == 3 && abs(y1 - y2) == 3, "Not a real cell")

  def nucleus: Point = Point(x1 + 1, y1 + 1)

  override def toString: String = "r("+ x1 + "," + y1 + ")-(" + x2 + "," + y2 + ") " + " Team: " + marker
}



/**  Virtual Cell **/
case class VCell(x1: Int, y1: Int,x2: Int, y2: Int, override val marker: Int) extends Cell {

  require((abs(x1 - x2) >= 3 && abs(y1 - y2) >= 3) && area != 9, "Not a virtual cell")

  override def toString: String = "v("+ x1 + "," + y1 + ")-(" + x2 + "," + y2 + ") " + " Team: " + marker
}




object RCell {}

object VCell {}

object Cell {}

