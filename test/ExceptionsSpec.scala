/**
  * Created by kevin on 04/02/19.
  */
import game.{Cell, Point, Team}
import model.{Board}
import org.scalatest.{FunSuite, PrivateMethodTester}

import scala.util.{Try}

class ExceptionSuite extends FunSuite with PrivateMethodTester  {

  test("BadMoveException: Move up") {

    val cell2 = Cell(17,17,19,19)
    val cell3 = Cell(7,7,10,10)

    val teamOne = Team(Cell(0,0,3,3), Cell(1,0,4,3), cell2, cell3)
    val teamTwo = Team.nullTeam

    val board = Board(teamOne, teamTwo)
    val up = PrivateMethod[Try[Board]]('up)

    val result = board invokePrivate up(Point(1, 1))
    assert(result.isFailure)

  }

  test("BadMoveException: Move left") {

    val cell2 = Cell(17,17,19,19)
    val cell3 = Cell(7,7,10,10)

    val teamOne = Team(Cell(0,0,3,3), Cell(1,0,4,3), cell2, cell3)
    val teamTwo = Team.nullTeam

    val board = Board(teamOne, teamTwo)
    val left = PrivateMethod[Try[Board]]('left)

    val result = board invokePrivate left(Point(1, 1))
    assert(result.isFailure)


  }

  test("BadMoveException: Move right") {

    val cell2 = Cell(17,17,19,19)
    val cell3 = Cell(7,7,10,10)

    val teamOne = Team(Cell(0,0,3,3), Cell(1,0,4,3), cell2, cell3)
    val teamTwo = Team.nullTeam

    val board = Board(teamOne, teamTwo)
    val right = PrivateMethod[Try[Board]]('right)

    val result = board invokePrivate right(Point(1, 1))
    assert(result.isFailure)

  }
}