package model

import com.google.gson.Gson
import game.{Cell, Coordinate, Team}

/**
  * Created by kevin on 29/01/19.
  */


case class Board(teamOne: Team, teamTwo: Team) extends BoardTrait {

  val dimensions = ((teamOne.cells ::: teamTwo.cells size) / 2) * 4

  def onBoard(i: Int): Boolean = i >= 0 && i < dimensions

  def isValidMove(cell: Cell): Boolean = {
    onBoard(cell.topLeft.x) && onBoard(cell.botRight.x) &&
      onBoard(cell.topLeft.y) && onBoard(cell.botRight.y)
  }

}

object Board {

  import play.api.libs.json._

  implicit val boardFormats = Json.format[Board]

  def writeBoard(board: Board): JsValue = {
    Json.toJson(board)
  }

  def readBoard(jsonBoard: JsValue): Board = {
    val teamOne = (jsonBoard \ "teamOne").as[Team]
    val teamTwo = (jsonBoard \ "teamTwo").as[Team]
    Board(teamOne, teamTwo)
  }

}
