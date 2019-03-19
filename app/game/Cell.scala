package game
import java.util.concurrent.atomic.AtomicBoolean

import breeze.linalg.{max, min}
import breeze.numerics.abs

import scala.collection.mutable.ListBuffer



/**
  * The Cell Trait
  */
sealed trait Cell {

  def id: Int = -1
  def x1: Int
  def y1: Int
  def x2: Int
  def y2: Int

  /**
    * The team assigned to the Cell
    * @return cell team (1 or 2)
    */
  def marker: Int = 0


  /**
    * Move Cell up
    * @return moved cell
    */
  def up: RCell = {
    RCell(x1 + 0, y1 - 1, x2 + 0, y2 - 1, marker, id)
  }

  /**
    * Move Cell down
    * @return moved cell
    */
  def down: RCell = {
    RCell(x1 + 0, y1 + 1, x2 + 0, y2 + 1, marker, id)
  }

  /**
    * Move Cell left
    * @return moved cell
    */
  def left: RCell = {
    RCell(x1 - 1, y1 + 0, x2 - 1, y2 + 0, marker, id)
  }

  /**
    * Move Cell right
    * @return moved cell
    */
  def right: RCell = {
    RCell(x1 + 1, y1 + 0, x2 + 1, y2 + 0, marker, id)
  }

  /**
    * Abstract
    * @param p_marker the team (1,2) to assign Cell
    * @return captured Cell
    */
  def capture(p_marker: Int): Cell

  /**
    * Get points for drawing cell
    * @return list of points need to print Cell
    */
  def drawPoints: List[Point] = {
    for {
      x <- x1 until x2
      y <- y1 until y2
    } yield Point(x, y)
  }.toList

  /**
    * Check if two Cells overlap
    * @note https://www.geeksforgeeks.org/find-two-rectangles-overlap/
    * @param other the other cell
    * @return true if overlapping, else false
    */
  def contains(other: Cell): Boolean = {
    if(x1 >= other.x2 || other.x1 >= x2) false
    else if(y1 >= other.y2 || other.y1 >= y2 ) false
    else true
  }

  /**
    * @note Replaced because very slow
    */
//  def contains(other: Cell): Boolean = {
//    this != other &&
//    allPoints.exists(point => other.innerPoints.contains(point))
//    }
//
//  def innerPoints: List[Point] = {
//    for {
//      x <- x1 + 1 until x2
//      y <- y1 + 1 until y2
//    } yield Point(x, y)
//  }.toList
//
//  def allPoints: List[Point] = {
//    for {
//      x <- x1 to x2
//      y <- y1 to y2
//    } yield Point(x, y)
//  }.toList


  /**
    * Area of a Cell
    * @return area of Cell
    */
  def area: Int = abs(x2 - x1) * abs(y2 - y1)


  /**
    * Merge two Cells
    * @param other the other cell
    * @return merged Cell
    */
  def merge(other: Cell): Cell = {
    //println(this + " with " + other)
    if(!(this contains other)) {
      Console.err.print(this + " \n" + other)
    }

    assert(this contains  other, "Shouldn't be merged")
    val topLeftX = min(x1, other.x1)
    val topLeftY = min(y1, other.y1)
    val botRightX = max(x2, other.x2)
    val botRightY = max(y2, other.y2)
    VCell(topLeftX, topLeftY,botRightX, botRightY, marker, id)
  }


  /**
    * Cell as a string
    * @return cell as a string
    */
  override def toString: String = " ("+ x1 + "," + y1 + ")-(" + x2 + "," + y2 + ") " + " Team: " + marker + " id:" + id
}



/**  Vertex Trait **/
sealed trait Vertex { var visited: AtomicBoolean = new AtomicBoolean(false) }



/**  Real Cell **/
case class RCell(x1: Int, y1: Int,x2: Int, y2: Int, override val marker: Int, override val id: Int = -1) extends Cell with Vertex {

  require(abs(x1 - x2) == 3 && abs(y1 - y2) == 3, "Not a real cell")

  /**
    * Assign new team to Cell
    * @param p_marker the team (1,2) to assign Cell
    * @return captured Cell
    */
  def capture(p_marker: Int): Cell = RCell(x1,y1,x2,y2,p_marker,id)


  /**
    * Assign new team to Cell
    * @return center point of Cell
    */
  def nucleus: Point = Point(x1 + 1, y1 + 1)


  /**
    * Cell as a string
    * @return cell as a string
    */
  override def toString: String = "r("+ x1 + "," + y1 + ")-(" + x2 + "," + y2 + ") " + " Team: " + marker + " id:" + id
}



/**  Virtual Cell **/
case class VCell(x1: Int, y1: Int,x2: Int, y2: Int, override val marker: Int, override val id: Int = -1) extends Cell {

  require((abs(x1 - x2) >= 3 && abs(y1 - y2) >= 3) && area != 9, "Not a virtual cell")


  /**
    * Assign new team to Cell
    * @param p_marker the team (1,2) to assign Cell
    * @return captured Cell
    */
  def capture(p_marker: Int): Cell = VCell(x1,y1,x2,y2,p_marker,id)

  /**
    * Cell as a string
    * @return cell as a string
    */
  override def toString: String = "v("+ x1 + "," + y1 + ")-(" + x2 + "," + y2 + ") " + " Team: " + marker + " id:" + id
}


object RCell {}

object VCell {}

object Cell {}

