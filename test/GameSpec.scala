import org.scalatestplus.play.PlaySpec
import game.{Cell, Coordinate, Team}
import model.Board
import org.scalatest.{FlatSpec, PrivateMethodTester}

import scala.util.Success


/**
  * Created by kevin on 25/04/17.
  */
class GameSpec extends PlaySpec with PrivateMethodTester {

  val cell1 = Cell(Coordinate(0,0),Coordinate(2,2))
  val cell2 = Cell(Coordinate(17,17),Coordinate(19,19))
  val cell3 = Cell(Coordinate(7,7), Coordinate(10,10))
  val cell4 = Cell(Coordinate(3,3), Coordinate(6,6))
  val cell5 = Cell(Coordinate(2,2), Coordinate(5,5))
  val cell6 = Cell(Coordinate(0,5), Coordinate(3,8))

  val teamOne = Team(List(cell1,cell2,cell3,cell1,cell2))
  val teamTwo = Team(List(cell4,cell5,cell6,cell1,cell2))
  val board = Board(teamOne, teamTwo)

  "real cells are like" should {
    "(3,3)-(6,6)" in {
      cell4.realCell mustEqual true
    }
    "(2,2)-(5,5)" in {
      cell5.realCell mustEqual true
    }
    "(0,5)-(3,8)" in {
      cell6.realCell mustEqual true
    }
  }

  "real cells are NOT like" should {
    "(0,5)-(3,8)" in {
      Cell(Coordinate(0,0), Coordinate(4,4)).realCell mustEqual false
    }
  }

  "nucleus is" should {
    "(8-8) in (7-7)-(10-10)" in {
      cell3.nucleus.getOrElse(None) mustEqual Coordinate(8,8)
    }
    "(4-4) in (3-3)-(6-6) in " in {
      cell4.nucleus.getOrElse(None) mustEqual Coordinate(4,4)
    }
  }

  "nucleus doesn't exist for" should {
    "(8-8) in (7-7)-(10-10)" in {
      Cell(Coordinate(0,0), Coordinate(4,4)).nucleus.getOrElse(None) mustEqual None
    }
  }

  "collision" should {
    val isCollision = PrivateMethod[Boolean]('isCollision)
    "happens for (2-2)-(5-5) and (2-2)-(5-5) teamOne " in {
      board invokePrivate isCollision(Cell(Coordinate(2,2), Coordinate(5,5))) mustEqual true
    }
    "doesn't happens for (0-0)-(3-3) and (0-0)-(2-2) in teamOne " in {
      board invokePrivate isCollision(Cell(Coordinate(0,0), Coordinate(3,3))) mustEqual false
    }
    "doesn't happens for (0-0)-(2-2) and (0-0)-(2-2) in teamOne " in {
      board invokePrivate isCollision(Cell(Coordinate(0,0), Coordinate(2,2))) mustEqual false
    }
    "doesn't happens for (4-4)-(7-7) and (3-3)-(6-6) in teamOne " in {
      board invokePrivate isCollision(Cell(Coordinate(4,4), Coordinate(7,7))) mustEqual false
    }
  }

  "allowable board values " should {
    val onBoard = PrivateMethod[Boolean]('onBoard)
    "are not < than 0" in {board invokePrivate onBoard(Cell(Coordinate(-1,-1), Coordinate(2,2))) mustBe false}
    "are not >= than the board length" in {board invokePrivate onBoard(Cell(Coordinate(0,-1), Coordinate(3,2))) mustBe false}
    "are >= than 0 and < dimensions" in {board invokePrivate onBoard(Cell(Coordinate(0,-0), Coordinate(3,3))) mustBe true}
  }

  "valid board overlapping moves for (0,1)-(3,4)" should {
    val teamOne = Team(List(Cell(Coordinate(0,1),Coordinate(3,4)), Cell(Coordinate(0,4),Coordinate(3,7)),cell2,cell3,cell2,cell3))
    val teamTwo = Team(List())
    val board = Board(teamOne, teamTwo)
    val down = PrivateMethod[Board]('down)
    "are move down" in  { board invokePrivate down(Coordinate(1,2)) mustEqual
      Success(Board(Team(List(Cell(Coordinate(0,2),Coordinate(3,5)), Cell(Coordinate(0,4),Coordinate(3,7)),cell2,cell3,cell2,cell3)),Team(List())))}
  }

  "valid moves for (0,0)-(2,2)" should {
      val isValidCellState = PrivateMethod[Boolean]('isValidCellState)
      "are not move up" in { board invokePrivate isValidCellState(cell1.up) mustBe false }
      "are not move left" in { board invokePrivate isValidCellState(cell1.left) mustBe false }
      "are move down" in  { board invokePrivate isValidCellState(cell1.down)  mustBe true }
      "are move right" in { board invokePrivate isValidCellState(cell1.right) mustBe true }
    }

  "valid moves for a(17,17)-(19,19)" should {
    val isValidCellState = PrivateMethod[Boolean]('isValidCellState)
    "are not move up"    in { board invokePrivate isValidCellState(cell2.up) mustBe true }
    "are not move left"  in { board invokePrivate isValidCellState(cell2.left) mustBe true }
    "are move down"  in { board invokePrivate isValidCellState(cell2.down) mustBe false }
    "are move right" in { board invokePrivate isValidCellState(cell2.right) mustBe false }
  }

  "valid moves for a(2,3)-(5,6)" should {
    val isValidCellState = PrivateMethod[Boolean]('isValidCellState)
    "are not move right"    in { board invokePrivate isValidCellState(Cell(Coordinate(2,3), Coordinate(5,6)).right) mustBe false }
    "are not move left"  in { board invokePrivate isValidCellState(Cell(Coordinate(2,3), Coordinate(5,6)).left) mustBe true }
    "are move down"  in { board invokePrivate isValidCellState(Cell(Coordinate(2,3), Coordinate(5,6)).down) mustBe true }
    "are move up" in { board invokePrivate isValidCellState(Cell(Coordinate(2,3), Coordinate(5,6)).up) mustBe false }
  }


  "inner cells of" should {
    "(3,3)-(6,6) are (4,4)-(5-5)" in {
      Cell(Coordinate(3, 3), Coordinate(6, 6)).innerCells mustEqual Cell(Coordinate(4, 4), Coordinate(5, 5)).outerCells
    }
    "(3,3)-(9,9) are (4,4)-(8,8)" in {
      Cell(Coordinate(3, 3), Coordinate(9, 9)).innerCells mustEqual Cell(Coordinate(4, 4), Coordinate(8, 8)).outerCells
    }
    "(0,0)-(9,3) are (1,1)-(8,2)" in {
      Cell(Coordinate(0, 0), Coordinate(9, 3)).innerCells mustEqual Cell(Coordinate(1, 1), Coordinate(8, 2)).outerCells
    }
  }
  "inner cells of" should {
    "(3,3)-(6,6) are not (4,4)-(4-6)" in {
      Cell(Coordinate(3,3), Coordinate(6,6)).innerCells == Cell(Coordinate(4,4), Coordinate(1,6)).outerCells mustBe false
    }
    "(3,3)-(9,9) are not (4,4)-(8,7)" in {
      Cell(Coordinate(3,3), Coordinate(9,9)).innerCells == Cell(Coordinate(4,4), Coordinate(1,7)).outerCells mustBe false
    }
    "(0,0)-(9,3) are not (1,1)-(8,2)" in {
      Cell(Coordinate(0,0), Coordinate(9,3)).innerCells == Cell(Coordinate(1,1), Coordinate(1,2)).outerCells mustBe false
    }
  }

  "(3,3)-(6,6) doesn't contain" should {
    "(0,0)-(3,3)" in {
      Cell(Coordinate(3, 3), Coordinate(6, 6)) contains Cell(Coordinate(0, 0), Coordinate(3, 3)) mustBe false
    }
    "(3,0)-(6,3)" in {
      Cell(Coordinate(3, 3), Coordinate(6, 6)) contains Cell(Coordinate(0, 0), Coordinate(3, 3)) mustBe false
    }
    "(6,0)-(9,3)" in {
      Cell(Coordinate(3, 3), Coordinate(6, 6)) contains Cell(Coordinate(0, 0), Coordinate(3, 3)) mustBe false
    }
    "(0,3)-(3,6)" in {
      Cell(Coordinate(3, 3), Coordinate(6, 6)) contains Cell(Coordinate(0, 0), Coordinate(3, 3)) mustBe false
    }
    "(6,3)-(9,6)" in {
      Cell(Coordinate(3, 3), Coordinate(6, 6)) contains Cell(Coordinate(0, 0), Coordinate(3, 3)) mustBe false
    }
    "(0,6)-(3,9)" in {
      Cell(Coordinate(3, 3), Coordinate(6, 6)) contains Cell(Coordinate(0, 0), Coordinate(3, 3)) mustBe false
    }
    "(3,6)-(6,9)" in {
      Cell(Coordinate(3, 3), Coordinate(6, 6)) contains Cell(Coordinate(0, 0), Coordinate(3, 3)) mustBe false
    }
    "(6,6)-(9,9)" in {
      Cell(Coordinate(3, 3), Coordinate(6, 6)) contains Cell(Coordinate(0, 0), Coordinate(3, 3)) mustBe false
    }
  }

  "(3,3)-(6,6) does contain" should {
    "(1,1)-(4,4)" in {
      Cell(Coordinate(3, 3), Coordinate(6, 6)) contains Cell(Coordinate(1, 1), Coordinate(4, 4)) mustBe true
    }
    "(1,1)-(4,4) reversed" in {
      Cell(Coordinate(1, 1), Coordinate(4, 4)) contains Cell(Coordinate(3, 3), Coordinate(6, 6)) mustBe true
    }
    "(2,1)-(5,4)" in {
      Cell(Coordinate(3, 3), Coordinate(6, 6)) contains Cell(Coordinate(2, 1), Coordinate(5, 4)) mustBe true
    }
    "(2,1)-(5,4) reversed" in {
      Cell(Coordinate(2, 1), Coordinate(5, 4)) contains Cell(Coordinate(3, 3), Coordinate(6, 6)) mustBe true
    }
    "(3,3)-(9,9)" in {
      Cell(Coordinate(3, 3), Coordinate(6, 6)) contains Cell(Coordinate(3, 3), Coordinate(9, 9)) mustBe true
    }
    "(3,3)-(9,9) reversed" in {
      Cell(Coordinate(3, 3), Coordinate(9, 9)) contains Cell(Coordinate(3, 3), Coordinate(6, 6)) mustBe true
    }
    "(5,5)-(8,8)" in {
      Cell(Coordinate(3, 3), Coordinate(6, 6)) contains Cell(Coordinate(5, 5), Coordinate(8, 8)) mustBe true
    }
    "(5,5)-(8,8) reversed" in {
      Cell(Coordinate(5, 5), Coordinate(8, 8)) contains Cell(Coordinate(3, 3), Coordinate(6, 6)) mustBe true
    }
    "(3,4)-(6,7)" in {
      Cell(Coordinate(3, 3), Coordinate(6, 6)) contains Cell(Coordinate(3, 4), Coordinate(6, 7)) mustBe true
    }
    "(3,4)-(6,7) reversed" in {
     Cell(Coordinate(3, 4), Coordinate(6, 7)) contains  Cell(Coordinate(3, 3), Coordinate(6, 6)) mustBe true
    }
  }

  "merging" should {
    "(0,0)-(3,3) merge (3,3)-(6,6) is None" in {
      Cell(Coordinate(0, 0), Coordinate(3, 3))  merge
        Cell(Coordinate(3, 3), Coordinate(6, 6))  mustBe  Cell(Coordinate(0, 0), Coordinate(6, 6)) }
    "(0,0)-(3,3) merge (2,2)-(5,5) is (0,0)-(5,5)" in {
      Cell(Coordinate(0, 0), Coordinate(3, 3)) merge
        Cell(Coordinate(2, 2), Coordinate(5, 5)) mustEqual
        Cell(Coordinate(0, 0), Coordinate(5, 5))  }
    "(0,0)-(3,3) merge (2,0)-(5,3) is (0,0)-(5,3)" in {
      Cell(Coordinate(0, 0), Coordinate(3, 3)) merge
        Cell(Coordinate(2, 0), Coordinate(5, 3)) mustEqual
        Cell(Coordinate(0, 0), Coordinate(5, 3))  }
    "(0,0)-(3,3) merge (2,2)-(5,5) merge (4,0)-(7,5) is (0,0)-(7,5)" in {
      (Cell(Coordinate(0, 0), Coordinate(3, 3)) merge
        Cell(Coordinate(2, 2), Coordinate(5, 5))) merge
        Cell(Coordinate(4, 0), Coordinate(7, 3)) mustEqual
        Cell(Coordinate(0, 0), Coordinate(7, 5))
    }
  }
  val team = new Team(List(cell1,cell2,cell3,cell4,cell5))
  val merged = cell4 merge cell5

  "merging teams" should {
    val merge = PrivateMethod[List[Cell]]('merge)
    "should give correct new team" in {
      board invokePrivate merge(team) mustEqual  (merged :: List(cell1,cell2,cell3))
    }
  }


//  val team2 = new Team(List(cell1,cell2,cell3,cell4,cell5,cell6))
//  val merged2 = (cell4 merge cell5) merge cell6
//  "recursively merging teams" should {
//    "should give correct new team" in {
//      board.recMerge(team2).cells mustEqual new Team(merged2 :: List(cell1,cell2,cell3)).cells
//    }
//  }

  val cell7 = Cell(Coordinate(7,8),Coordinate(10,11))
  "(7-7)-(10-10) moved down is" should {
    "(7-8)-(10-11)" in {
      Board(Team(List(cell1,cell2,cell3,cell4,cell5,cell6)), Team(List()))
        .down(Coordinate(8,8)) mustEqual Success(Board(Team(List(cell7,cell1,cell2,cell4,cell5,cell6)), Team(List())))
    }
  }

  val cell8 = Cell(Coordinate(7,6),Coordinate(10,9))
  "(7-7)-(10-10) moved up is" should {
    "(7-6)-(10-9)" in {
      Board(Team(List(cell1,cell2,cell3,cell4,cell5,cell6)), Team(List()))
        .up(Coordinate(8,8)) mustEqual Success(Board(Team(List(cell8,cell1,cell2,cell4,cell5,cell6)), Team(List())))
    }
  }

  val cell9 = Cell(Coordinate(6,7),Coordinate(9,10))
  "(7-7)-(10-10) moved left is" should {
    "(6-7)-(9-10)" in {
      Board(Team(List(cell1,cell2,cell3,cell4,cell5,cell6)), Team(List()))
        .left(Coordinate(8,8)) mustEqual Success(Board(Team(List(cell9,cell1,cell2,cell4,cell5,cell6)), Team(List())))
    }
  }

  val cell10 = Cell(Coordinate(8,7),Coordinate(11,10))
  "(7-7)-(10-10) moved right is" should {
    "(8-7)-(11-10)" in {
      Board(Team(List(cell1,cell2,cell3,cell4,cell5,cell6)), Team(List()))
        .right(Coordinate(8,8)) mustEqual Success(Board(Team(List(cell10,cell1,cell2,cell4,cell5,cell6)), Team(List())))
    }
  }



}
