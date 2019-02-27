package model

//import game.Team

/**
  * Created by kevin on 31/01/19.
  */
case class Move(x: Int, y: Int, move: String)


object Move {

  import play.api.libs.json._

  implicit val move = Json.format[Move]

  def writeMove(move: Move): JsValue = {
    Json.toJson(move)
  }

  def readMove(move: JsValue): Move = {
    val x = (move \ "x").as[Int]
    val y = (move \ "y").as[Int]
    val direction = (move \ "move").as[String]
    Move(x, y, direction)
  }
}