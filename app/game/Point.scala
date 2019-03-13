package game

/**
  * The Point representation.
  * @param x the x coordinate of a Point
  * @param y the y coordinate of a Point
  */
case class Point(x: Int, y: Int){

  /**
    * Point as a string
    * @return cell as a point
    */
  override def toString: String = "("+x+","+y+")"
}


object Point {

  import play.api.libs.json._

  implicit val coordinateFormats = Json.format[Point]

  def writeCoordinate(point: Point): JsValue = {
    Json.toJson(point)
  }

  def readTeam(jsonPoint: JsValue): Point = {
    val x = (jsonPoint \ "x").as[Int]
    val y = (jsonPoint \ "y").as[Int]
    Point(x,y)
  }



}


