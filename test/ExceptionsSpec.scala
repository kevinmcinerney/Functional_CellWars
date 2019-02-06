/**
  * Created by kevin on 04/02/19.
  */
import game.{Cell, Coordinate, Team}
import model.{BadMoveException, Board}
import org.scalatest.{FunSuite, PrivateMethodTester}
import org.scalatest.PrivateMethodTester.PrivateMethod

import scala.collection.mutable.Stack

class ExceptionSuite extends FunSuite with PrivateMethodTester  {

  test("BadMoveException: Move up") {


    val cell2 = Cell(Coordinate(17, 17), Coordinate(19, 19))
    val cell3 = Cell(Coordinate(7, 7), Coordinate(10, 10))

    val teamOne = Team(List(Cell(Coordinate(0, 0), Coordinate(3, 3)), Cell(Coordinate(1, 0), Coordinate(4, 3)), cell2, cell3))
    val teamTwo = Team(List())
    val board = Board(teamOne, teamTwo)
    val up = PrivateMethod[Board]('up)

    val b = new Board(teamOne, teamTwo)
    intercept[BadMoveException] {
      board invokePrivate up(Coordinate(1, 1))
    }
    assert(b == Board(teamOne, teamTwo))

  }

  test("BadMoveException: Move left") {


    val cell2 = Cell(Coordinate(17, 17), Coordinate(19, 19))
    val cell3 = Cell(Coordinate(7, 7), Coordinate(10, 10))

    val teamOne = Team(List(Cell(Coordinate(0, 0), Coordinate(3, 3)), Cell(Coordinate(1, 0), Coordinate(4, 3)), cell2, cell3))
    val teamTwo = Team(List())
    val board = Board(teamOne, teamTwo)
    val left = PrivateMethod[Board]('left)

    val b = new Board(teamOne, teamTwo)
    intercept[BadMoveException] {
      board invokePrivate left(Coordinate(1, 1))
    }
    assert(b == Board(teamOne, teamTwo))

  }

  test("BadMoveException: Move right") {


    val cell2 = Cell(Coordinate(17, 17), Coordinate(19, 19))
    val cell3 = Cell(Coordinate(7, 7), Coordinate(10, 10))

    val teamOne = Team(List(Cell(Coordinate(0, 0), Coordinate(3, 3)), Cell(Coordinate(1, 0), Coordinate(4, 3)), cell2, cell3))
    val teamTwo = Team(List())
    val board = Board(teamOne, teamTwo)
    val right = PrivateMethod[Board]('right)

    val b = new Board(teamOne, teamTwo)
    intercept[BadMoveException] {
      board invokePrivate right(Coordinate(1, 1))
    }
    assert(b == Board(teamOne, teamTwo))

  }
}