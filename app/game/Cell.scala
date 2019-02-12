package game

import breeze.linalg.{max, min}
import breeze.math.MutablizingAdaptor.CoordinateFieldAdaptor
import breeze.numerics.abs
import model.Board
import play.api.libs.json._

import scala.util.{Success, Try}


/**
  * Created by kevin on 29/04/17.
  */

sealed trait Cell {

  def x1: Int
  def y1: Int
  def x2: Int
  def y2: Int


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

  def contains(other: Cell): Boolean =
    this != other &&
    allPoints.exists(point => other.innerPoints.contains(point))

  def contains(other: Point): Boolean = {
    allPoints.contains(other)
  }

  def fullyInsideOf(other: Cell): Boolean = {
    this != other &&
    allPoints.forall(p => other.contains(p))
  }

  def merge(other: Cell): VCell = {
    assert(this contains  other, "Shouldn't be merged")
    val topLeftX = min(x1, other.x1)
    val topLeftY = min(y1, other.y1)
    val botRightX = max(x2, other.x2)
    val botRightY = max(y2, other.y2)
    VCell(topLeftX, topLeftY,botRightX, botRightY)
  }

  def area: Int = abs(x2 - x1) * abs(y2 - y1)

  def compare(other: Cell): Cell = {
    if(this.area > other.area) this
    else if (other.area > this.area) other
    else this
  }

  override def toString: String = " ("+ x1 + "," + y1 + ")-(" + x2 + "," + y2 + ") "
}

sealed trait Movable {

  def x1: Int
  def y1: Int
  def x2: Int
  def y2: Int

  def up: RCell = {
    RCell(x1 + 0, y1 - 1, x2 + 0, y2 - 1)
  }

  def down: RCell = {
    RCell(x1 + 0, y1 + 1, x2 + 0, y2 + 1)
  }

  def left: RCell = {
    RCell(x1 - 1, y1 + 0, x2 - 1, y2 + 0)
  }

  def right: RCell = {
    RCell(x1 + 1, y1 + 0, x2 + 1, y2 + 0)
  }

  def nucleus: Point = {
    Point(x1 + 1, y1 + 1)
  }
}


case class RCell(x1: Int, y1: Int,x2: Int, y2: Int) extends Cell with Movable {

  require(abs(x1 - x2) == 3 && abs(y1 - y2) == 3, "Not a real cell")

  override def toString: String = "r("+ x1 + "," + y1 + ")-(" + x2 + "," + y2 + ") "

//  implicit object RCellFormat extends Format[RCell] {
//
//    def reads(json: JsValue): RCell = RCell(
//      (json \ "x1").as[Int],
//      (json \ "y1").as[Int],
//      (json \ "x2").as[Int],
//      (json \ "y2").as[Int]
//    )
//
//    def writes(s: RCell): JsValue = JsObject(Seq(
//      "x1" -> JsNumber(s.x1),
//      "y1" -> JsNumber(s.y1),
//      "x2" -> JsNumber(s.x2),
//      "y2" -> JsNumber(s.y2)
//    ))
//  }
}

case class VCell(x1: Int, y1: Int,x2: Int, y2: Int) extends Cell {

  require((abs(x1 - x2) >= 3 && abs(y1 - y2) >= 3) && area != 9, "Not a virtual cell")

  override def toString: String = "v("+ x1 + "," + y1 + ")-(" + x2 + "," + y2 + ") "

//  implicit object VCellFormat extends Format[VCell] {
//
//    def reads(json: JsValue): VCell = VCell(
//      (json \ "x1").as[Int],
//      (json \ "y1").as[Int],
//      (json \ "x2").as[Int],
//      (json \ "y2").as[Int]
//    )
//
//    def writes(s: VCell): JsValue = JsObject(Seq(
//      "x1" -> JsNumber(s.x1),
//      "y1" -> JsNumber(s.y1),
//      "x2" -> JsNumber(s.x2),
//      "y2" -> JsNumber(s.y2)
//    ))
//  }

}

//import play.api.libs.json._
//import play.api.libs.functional.syntax._
//
//object Cell {
//
//  implicit val readCell =
//    __.read[RCell].map(x => x:Cell) orElse __.read[VCell].map(x => x:Cell)
//
//  implicit val writeCell = Writes[Cell]{
//    case r: RCell => RCell().RCellFormat.reads(r)
//    case v: VCell => VCell.writeVCell.writes(v)
//  }
//}


object RCell {

//  implicit val readRCell = Json.reads[RCell]
//
//  val writeRCell = Json.writes[RCell]

}

object VCell {

//  implicit val readVCell = Json.reads[VCell]
//
//  val writeVCell = Json.writes[VCell]

}
