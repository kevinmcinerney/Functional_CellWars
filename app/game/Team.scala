package game

import model.Board

/**
  * Created by kevin on 30/04/17.
  */

case class Team(cells: List[Cell]) {

  def size = cells.size

  def allPairs(n: Int): List[List[Cell]] = cells combinations(n) toList

  def contains(nucleus: Point): Boolean = {
    cells.exists(_.nucleus.getOrElse("None") == nucleus)
  }

}

object Team {

  def apply(cell : Cell*): Team =
    Team(cell.toList)

  def nullTeam: Team = Team(List())

  import play.api.libs.json._

  implicit val teamFormats = Json.format[Team]

  def writeTeam(team: Team): JsValue = {
    Json.toJson(team)
  }

  def readTeam(jsonTeam: JsValue): Team = {
    val cells = (jsonTeam \ "cells").as[List[Cell]]
    Team(cells)
  }
}
