/**
  * Created by kevin on 04/02/19.
  */
import game._
import model.Board
import org.scalatest.{FunSuite, PrivateMethodTester}

import scala.util.Try

class ExceptionSuite extends FunSuite with PrivateMethodTester  {

  test("BadMoveException: Move up") {

    val cell2 = RCell(17,17,20,20)
    val cell3 = RCell(7,7,10,10)

    val teamOne = Team(RCell(0,0,3,3), RCell(1,0,4,3), cell2, cell3)
    val teamTwo = Team.nullTeam

    val board = Board(teamOne, teamTwo)
    val up = PrivateMethod[Try[Board]]('up)

    val result = board invokePrivate up(Point(1, 1))
    assert(result.isFailure)

  }

  test("BadMoveException: Move left") {

    val cell2 = RCell(17,17,20,20)
    val cell3 = RCell(7,7,10,10)

    val teamOne = Team(RCell(0,0,3,3), RCell(1,0,4,3), cell2, cell3)
    val teamTwo = Team.nullTeam

    val board = Board(teamOne, teamTwo)
    val left = PrivateMethod[Try[Board]]('left)

    val result = board invokePrivate left(Point(1, 1))
    assert(result.isFailure)


  }

  test("BadMoveException: Move right") {

    val cell2 = RCell(17,17,20,20)
    val cell3 = RCell(7,7,10,10)

    val teamOne = Team(RCell(0,0,3,3), RCell(1,0,4,3), cell2, cell3)
    val teamTwo = Team.nullTeam

    val board = Board(teamOne, teamTwo)
    val right = PrivateMethod[Try[Board]]('right)

    val result = board invokePrivate right(Point(1, 1))
    assert(result.isFailure)

  }

  test("BadMergeAssertion") {

    val cell3 = RCell(2,3,5,6)
    val cell4 = RCell(5,5,8,8)

    intercept[java.lang.AssertionError] {
      cell3 merge cell4
    }
  }

  test("Bad Real Cell") {

    intercept[java.lang.IllegalArgumentException] {
      val test = RCell(0,0,2,2)
    }

    intercept[java.lang.IllegalArgumentException] {
      val test = RCell(0,0,4,4)
    }

    intercept[java.lang.IllegalArgumentException] {
      val test = RCell(0,0,3,4)
    }

    intercept[java.lang.IllegalArgumentException] {
      val test = RCell(0,0,4,3)
    }
  }

  test("Bad Virtual Cell") {

    intercept[java.lang.IllegalArgumentException] {
      val test = VCell(0,0,2,2)
    }

    intercept[java.lang.IllegalArgumentException] {
      val test = VCell(0,0,3,3)
    }

    intercept[java.lang.IllegalArgumentException] {
      val test = VCell(0,3,2,8)
    }

    intercept[java.lang.IllegalArgumentException] {
      val test = VCell(3,0,8,2)
    }
  }

}