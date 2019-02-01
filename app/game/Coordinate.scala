package game

/**
  * Created by kevin on 29/04/17.
  */
case class Coordinate(x: Int, y: Int)

object Coordinate{

  import play.api.libs.json._

  implicit val coordinateFormats = Json.format[Coordinate]

  def writeCoordinate(point: Coordinate): JsValue = {
    Json.toJson(point)
  }

  def readTeam(jsonPoint: JsValue): Coordinate = {
    val x = (jsonPoint \ "x").as[Int]
    val y = (jsonPoint \ "y").as[Int]
    Coordinate(x,y)
  }

}

