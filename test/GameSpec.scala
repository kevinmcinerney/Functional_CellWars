import org.scalatestplus.play.PlaySpec
import game.{Cell, Coordinate, Team}
import model.Board


/**
  * Created by kevin on 25/04/17.
  */
class GameSpec extends PlaySpec {

  val board = Board(20)
  val cell1 = Cell(Coordinate(0,0),Coordinate(2,2))
  val cell2 = Cell(Coordinate(17,17),Coordinate(19,19))
  val cell3 = Cell(Coordinate(7,7), Coordinate(10,10))
  val cell4 = Cell(Coordinate(3,3), Coordinate(6,6))
  val cell5 = Cell(Coordinate(2,2), Coordinate(5,5))


  "allowable board values " should {
    "are not < than 0" in {(-10 to -1).map(n => board.onBoard(n)).forall(_ == false) mustBe true }
    "are not >= than the board length" in {(board.dimensions to 30).map(n => board.onBoard(n)).forall(_ == false) mustBe true }
    "are >= than 0 and < dimensions" in {(0 until board.dimensions).map(n => board.onBoard(n)).forall(_ == true) mustBe true }
  }

  "valid moves for (0,0)-(2,2)" should {
      "are not move up" in { board isValidMove cell1.up mustBe false }
      "are not move left" in { board isValidMove cell1.left mustBe false }
      "are move down" in  { board isValidMove cell1.down  mustBe true }
      "are move right" in { board isValidMove cell1.right mustBe true }
    }

  "valid moves for a(17,17)-(19,19)" should {
    "re not move up"    in { board isValidMove cell2.up mustBe true }
    "are not move left"  in { board isValidMove cell2.left mustBe true }
    "are move down"  in { board isValidMove cell2.down mustBe false }
    "are move right" in { board isValidMove cell2.right mustBe false }
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
        Cell(Coordinate(3, 3), Coordinate(6, 6)) equals None mustBe true }
    "(0,0)-(3,3) merge (2,2)-(5,5) is (0,0)-(5,5)" in {
      Cell(Coordinate(0, 0), Coordinate(3, 3)) merge
        Cell(Coordinate(2, 2), Coordinate(5, 5)) mustEqual
        Some(Cell(Coordinate(0, 0), Coordinate(5, 5)))  }
    "(0,0)-(3,3) merge (2,0)-(5,3) is (0,0)-(5,3)" in {
      Cell(Coordinate(0, 0), Coordinate(3, 3)) merge
        Cell(Coordinate(2, 0), Coordinate(5, 3)) mustEqual
        Some(Cell(Coordinate(0, 0), Coordinate(5, 3)))  }
    "(0,0)-(3,3) merge (2,2)-(5,5) merge (4,0)-(7,5) is (0,0)-(7,5)" in {
      (Cell(Coordinate(0, 0), Coordinate(3, 3)) merge
        Cell(Coordinate(2, 2), Coordinate(5, 5))).get merge
        Cell(Coordinate(4, 0), Coordinate(7, 3)) mustEqual
        Some(Cell(Coordinate(0, 0), Coordinate(7, 5)))
    }
  }
  val team = new Team(List(cell1,cell2,cell3,cell4,cell5))
  val merged = cell4 merge cell5

  "merging teams" should {
    "should give correct new team" in {
     team.merge(team) mustEqual  (merged.get :: List(cell1,cell2,cell3))
    }
  }

  val cell6 = Cell(Coordinate(0,5), Coordinate(3,8))
  val team2 = new Team(List(cell1,cell2,cell3,cell4,cell5,cell6))
  val merged2 = (cell4 merge cell5).get merge cell6
  "recursively merging teams" should {
    "should give correct new team" in {
      team2.recMerge(team2).cells mustEqual  new Team(merged2.get :: List(cell1,cell2,cell3)).cells
    }
  }

}
