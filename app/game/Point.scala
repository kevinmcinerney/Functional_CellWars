package game

/**
  * Created by kevin on 29/04/17.
  */
case class Point(x: Int, y: Int){
  override def toString: String = "("+x+","+y+")"
}



object Point{

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

