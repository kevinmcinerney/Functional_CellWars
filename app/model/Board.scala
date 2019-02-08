package model

import com.google.gson.Gson
import game.{Cell, Point, Team}

import scala.util.Try

/**
  * Created by kevin on 29/01/19.
  */

class BadMoveException(msg: String) extends Exception(msg)

case class Board(teamOne: Team, teamTwo: Team, merged: Team = Team.nullTeam){


  def dimensions: Int = {
    ((teamOne.cells ::: teamTwo.cells size) / 2) * 4
  }

  def up(point: Point): Try[Board] = {
    move(up)(point)
  }

  def down(point: Point): Try[Board] = {
    move(down)(point)
  }

  def left(point: Point): Try[Board] = {
    move(left)(point)
  }

  def right(point: Point): Try[Board] = {
    move(right)(point)
  }

  private def up(team: Team, nucleus: Point): Team = {
    move(_.up, team, nucleus)
  }

  private def right(team: Team, nucleus: Point): Team = {
    move(_.right, team, nucleus)
  }

  private def down(team: Team, nucleus: Point): Team = {
    move(_.down, team, nucleus)
  }

  private def left(team: Team, nucleus: Point): Team = {
      move(_.left, team, nucleus)
  }

  private def move(move: (Team, Point) => Team)(nucleus: Point): Try[Board] = {

    if (teamOne.contains(nucleus))
      Try(recMerge(Board(move(teamOne, nucleus), teamTwo)))
    else if (teamTwo.contains(nucleus))
      Try(recMerge(Board(teamOne, move(teamTwo, nucleus))))
    else
      Try(recMerge(Board(teamOne, teamTwo)))

  }

  private def move(move: Cell => Cell, team: Team, nucleus: Point): Team = {

    val (mover, others) = team.cells.partition(_.nucleus == Some(nucleus))

    val newCell = move(mover.head)

    if(mover.nonEmpty)
      if(isValidCellState(newCell)) Team(newCell :: others)
      else throw new BadMoveException("Invalid move from this position")
    else throw new BadMoveException("You didn't select a cell to move")

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
    allCells(this).exists(c => c.realCell && c == cell)
  }

  private def allCells(board: Board): List[Cell] = board.teamOne.cells ::: board.teamTwo.cells ::: board.merged.cells

  def recMerge(board: Board): Board = {

    val mergedCells = merge(board) // cache

    println(mergedCells.nonEmpty)
    if (mergedCells.nonEmpty) {

      recMerge(Board(Team(board.teamOne.cells),
                     Team(board.teamTwo.cells),
                     Team(mergedCells)))
    }
    else board
  }

  private def unmergedCells(board: Board): List[Cell] = {
    allCells(board)
      .filterNot(c1 => board.merged.cells
        .exists(c2  => c1.fullyInsideOf(c2))) :::
      board.merged.cells
  }


  private def merge(board: Board): List[Cell] = {
    mergeCellPairs(findMergePairs(board))
  }

  private def findMergePairs(board: Board): List[List[Cell]] = {
    unmergedCells(board)
      .combinations(2)
      .filter(pair => (pair.head contains pair.tail.head)
                   && (pair.head != pair.tail.head)).toList
  }

  private def mergeCellPairs(cellPairs: List[List[Cell]]): List[Cell] = {
    cellPairs.map(pair => pair.head merge pair.last)
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
