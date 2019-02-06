package model

import com.google.gson.Gson
import game.{Cell, Coordinate, Team}

import scala.util.Try

/**
  * Created by kevin on 29/01/19.
  */

class BadMoveException(msg: String) extends Exception(msg) {}

case class Board(teamOne: Team, teamTwo: Team){


  def dimensions: Int = {
    ((teamOne.cells ::: teamTwo.cells size) / 2) * 4
  }

  def up(point: Coordinate): Try[Board] = {
    move(up)(point)
  }

  def down(point: Coordinate): Try[Board] = {
    move(down)(point)
  }

  def left(point: Coordinate): Try[Board] = {
    move(left)(point)
  }

  def right(point: Coordinate): Try[Board] = {
    move(right)(point)
  }

  private def up(team: Team, nucleus: Coordinate): Team = {
    move(Cell.up, team, nucleus)
  }

  private def right(team: Team, nucleus: Coordinate): Team = {
    move(Cell.right, team, nucleus)
  }

  private def down(team: Team, nucleus: Coordinate): Team = {
    move(Cell.down, team, nucleus)
  }

  private def left(team: Team, nucleus: Coordinate): Team = {
      move(Cell.left, team, nucleus)
  }

  private def move(move: Cell => Cell, team: Team, nucleus: Coordinate): Team = {

    val (mover, others) = team.cells.partition(_.nucleus == Some(nucleus))

    val newCell = move(mover.head)

    if(mover.nonEmpty){
      if(isValidCellState(newCell)) {
        Team(newCell :: others)
      }else{
        throw new BadMoveException("Invalid move from this position")
      }
    }else{
      throw new BadMoveException("You didn't select a cell to move")
    }
  }

  private def move(move: (Team, Coordinate) => Team)(nucleus: Coordinate): Try[Board] = {

    if(teamOne.cells.exists(_.nucleus.getOrElse("None") == nucleus)){
      Try(Board(move(teamOne, nucleus), teamTwo))
    }
    else if (teamTwo.cells.exists(_.nucleus.getOrElse("None") == nucleus)) {
      Try(Board(teamOne, move(teamTwo, nucleus)))
    }
    else{
      Try(Board(teamOne, teamTwo))
    }
  }

  private def onBoard(cell: Cell): Boolean = {

    val inRange = (i: Int) => i >= 0 && i < dimensions

    inRange(cell.topLeft.x) && inRange(cell.botRight.x) &&
      inRange(cell.topLeft.y) && inRange(cell.botRight.y)

  }

  private def isValidCellState(cell: Cell): Boolean = {
    onBoard(cell) &&
    !isCollision(cell)

  }

  private def isCollision(cell: Cell): Boolean = {

    val allCells = teamOne.cells ::: teamTwo.cells

    allCells.exists(c => c.realCell && c == cell)
  }

  def recMerge(team: Team): Team = {
    if (mergedNeeded(team)) recMerge(new Team(this.merge(team)))
    else team
  }

  def allCells: Team = Team(teamOne.cells ::: teamTwo.cells)


  private def mergedNeeded(team: Team): Boolean = {
    team.allPairs(2)
      .exists(pair => pair.head contains pair.last)
  }

  private def mergeCellPairs(cellPairs: List[List[Cell]]): List[Cell] = {
    cellPairs.map(pair => pair.head merge pair.last)
  }

  private def merge(team: Team): List[Cell] = {
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
