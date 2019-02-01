package game

import model.Board

/**
  * Created by kevin on 30/04/17.
  */


case class Team(cells: List[Cell]) {

  def size = cells.size

  def allPairs(n: Int): List[List[Cell]] = this.cells.combinations(n).toList

}

object Team{

  def mergedNeeded(team: Team): Boolean = {
    team.allPairs(2)
      .exists(pair => pair.head contains pair.last)
  }

  def recMerge(team: Team): Team = {
    if (mergedNeeded(team)) recMerge(new Team(this.merge(team)))
    else team
  }

  def mergeCellPairs(cellPairs: List[List[Cell]]): List[Cell] = {
    cellPairs.map(pair => pair.head merge pair.last get)
  }

  def merge(team: Team): List[Cell] = {
    val (merged, unmerged) = team
      .allPairs(2)
      .partition(pair => pair.head contains pair.last)

    val mergedCells = mergeCellPairs(merged)

    val unmergedCells = unmerged
      .flatten
      .filterNot(merged.flatten.contains(_))
      .distinct

    mergedCells ::: unmergedCells
  }

  def up(team: Team, point: Coordinate): Team = {
    val (mover, others) = team.cells.partition(_.contains(point))
    Team(mover.head.up :: others)
  }

  def right(team: Team, point: Coordinate): Team = {
    val (mover, others) = team.cells.partition(_.contains(point))
    Team(mover.head.right :: others)
  }

  def down(team: Team, point: Coordinate): Team = {
    val (mover, others) = team.cells.partition(_.contains(point))
    println(mover)
    println(others)
    Team(mover.head.down :: others)
  }

  def left(team: Team, point: Coordinate): Team = {
    val (mover, others) = team.cells.partition(_.contains(point))
    Team(mover.head.left :: others)
  }

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
