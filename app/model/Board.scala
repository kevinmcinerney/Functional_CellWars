package model

import com.google.gson.Gson
import game._

import scala.None
import scala.util.{Try}

/**
  * Created by kevin on 29/01/19.
  */

class BadMoveException(msg: String) extends Exception(msg)

case class Board(teamOne: Team, teamTwo: Team){


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

  def recMerge(board: Board, mover: Int): Board = {
    getTeamHead(board, mover) match {
      case Some(head) if mover == 1 =>
        recMerge(Board(Team(replaceHead(head :: board.teamOne.cells)),
                       Team(board.teamTwo.cells)), mover)
      case Some(head) if mover == 2 =>
        recMerge(Board(Team(board.teamOne.cells),
                       Team(replaceHead(head :: board.teamTwo.cells))), mover)
      case None => board
    }
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
  private  def getTeam(board: Board, team: Int): Team ={
    if (team == 1) board.teamOne else board.teamTwo
  }

  private def move(move: (Team, Point) => Team)(nucleus: Point): Try[Board] = {
    if (teamOne.contains(nucleus))
      Try(recMerge(Board(move(teamOne, nucleus), teamTwo),1))
    else if (teamTwo.contains(nucleus))
      Try(recMerge(Board(teamOne, move(teamTwo, nucleus)),2))
    else
      Try(Board(teamOne, teamTwo))
  }

  private def move(move: RCell => RCell, team: Team, nucleus: Point): Team = {
    val (mover, tail) = team.realCells.partition(_.nucleus == nucleus)
    val head = move(mover.head)
    if(mover.nonEmpty)
      if(isValidCellState(head)) Team(head :: tail)
      else throw new BadMoveException("Invalid move from this position")
    else throw new BadMoveException("You didn't select a cell to move")
  }

  private def onBoard(cell: Cell): Boolean = {
    val inRange = (i: Int) => i >= 0 && i < dimensions
    inRange(cell.x1) && inRange(cell.x2) &&
      inRange(cell.y1) && inRange(cell.y2)
  }

  private def isValidCellState(cell: Cell): Boolean = {
    onBoard(cell) && !isCollision(cell)
  }

  private def isCollision(cell: Cell): Boolean = {
    allRealCells(this).contains(cell)
  }

  private def allCells(board: Board): List[Cell] = {
    board.teamOne.cells ::: board.teamTwo.cells
  }

  private def allRealCells(board: Board): List[RCell] = {
    allCells(board).collect { case a: RCell => a }
  }

  private def allVirtualCells(board: Board): List[VCell] = {
    allCells(board).collect { case a: VCell => a }
  }

  private def getTeamHead(board: Board, mover: Int): Option[VCell] = {
    getCellTuple(board, mover) match {
      case Right(tuple) => Some(mergeCellPair(tuple))
      case Left(msg) => None
    }
  }

  /* New implementation O(n+1)^2 instead of 0(mn)^2 */
  private def getCellTuple(board: Board, mover: Int): Either[String, (Cell, Cell)] = {
    val team = getTeam(board, mover)
    val canMergeCells = outerCells(board, team.cells.head)
    val canMergePairs = canMergeCells.map(c => (canMergeCells.head, c))
      .filter(p => p._1 contains p._2)

    assert(canMergePairs.size <= 2, "Outer cells should only find a max of two merges at a time")

    canMergePairs match {
      case x :: y :: Nil => Right(mergeCellPair(x), mergeCellPair(y))
      case x :: Nil => Right(x)
      case Nil => Left("No merge pair found")
    }
  }

  private def outerCells(board: Board, mover: Cell): List[Cell] = {
    reduce(mover :: allCells(board), allVirtualCells(board))
  }

  private def mergeCellPair(cellPair: (Cell,Cell)): VCell = {
    cellPair._1 merge cellPair._2
  }

  private def replaceHead(cells: List[Cell]): List[Cell] = cells match {
    case (x: RCell) :: (y: RCell) :: xs => x :: y :: xs
    case (x: VCell) :: (y: RCell) :: xs => x :: y :: xs
    case (x: VCell) :: (y: VCell) :: xs => x :: xs
  }

  private def reduce(l1: List[Cell], l2: List[Cell]): List[Cell] = {
    l1.filter(c1 => l2.forall(c2 => !(c1 fullyInsideOf c2)))
  }

  private def to2DArray(mover: Int): Array[Array[Int]] = {
    val matrix = Array.fill[Int](dimensions,dimensions) { 0 }

    def addTeam(team: Team, id: Int): Unit ={
      for{ c <- team.cells
           p <- c.allPoints
      } matrix(p.x)(p.y) = id
    }

    def addNucleus(team: Team, id: Int): Unit ={
      for{ c <- team.realCells } matrix(c.x1 + 1)(c.y1 + 1) = id * 10
    }

    addTeam(teamOne, 1)
    addNucleus(teamOne, 1)

    addTeam(teamOne, 2)
    addNucleus(teamOne, 2)

    matrix

  }

}

object Board {

//  import play.api.libs.json._
//
//  implicit val boardFormats = Json.format[Board]
//
//  def writeBoard(board: Board): JsValue = {
//    Json.toJson(board)
//  }
//
//  def readBoard(jsonBoard: JsValue): Board = {
//    val teamOne = (jsonBoard \ "teamOne").as[Team]
//    val teamTwo = (jsonBoard \ "teamTwo").as[Team]
//    Board(teamOne, teamTwo)
//  }

}
