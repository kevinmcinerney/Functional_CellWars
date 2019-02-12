import org.scalatestplus.play.PlaySpec
import game._
import model.Board
import org.scalatest.{FlatSpec, PrivateMethodTester}

import scala.util.Success


/**
  * Created by kevin on 25/04/17.
  */
class GameSpec extends PlaySpec with PrivateMethodTester {

  val cell1 = RCell(0,0,3,3)
  val cell2 = RCell(3,0,6,3)
  val cell3 = RCell(2,3,5,6)
  val cell4 = RCell(5,5,8,8)
  val cell5 = RCell(2,7,5,10)
  val cell6 = RCell(6,0,9,3)
  val cell7 = RCell(9,2,12,5)

  val cell8 = RCell(9,9,12,12)
  val cell9 = RCell(17,0,20,3)
  val cell10 = RCell(17,6,20,9)
  val cell11 = RCell(17,9,20,12)
  val cell12 = RCell(17,12,20,15)
  val cell13 = RCell(12,12,15,15)
  val cell14 = RCell(15,15,18,18)

  val team1 = Team(cell1,cell2,cell3,cell4,cell5,cell6, cell7)
  val team2 = Team(cell8,cell9,cell10,cell11,cell12,cell13,cell14)
  val board_t1 = Board(team1, team2)

  val team3 = Team(cell3.right,cell1,cell2,cell4,cell5,cell6, cell7)
  val team4 = Team(cell8,cell9,cell10,cell11,cell12,cell13,cell14)
  val board_t2 = Board(Team(VCell(2,3,8,10) :: team3.cells), team4)

  val team5 = Team(cell1,cell2,cell3.right,cell4,cell5.right,cell6, cell7)
  val team6 = Team(cell8,cell9,cell10,cell11,cell12,cell13,cell14)
  val board_t3 = Board(team5, team6)

  val team7 = Team(cell1,cell2,cell3.right,cell4,cell5.right,cell6.right, cell7)
  val team8 = Team(cell8,cell9,cell10,cell11,cell12,cell13,cell14)
  val board_t4 = Board(team7, team8)

  val team9 =  Team(team1.cells)
  val team10 = Team(VCell(0,0,4,4) :: team1.cells)
  val team11 = Team(VCell(0,0,4,4) :: VCell(4,4,8,8) :: team1.cells)


  "nucleus is" should {
    Point(1,1) + " in " + cell1 in {
      cell1.nucleus mustEqual Point(1,1)
    }
    Point(4,1) + " in " + cell2 in {
      cell2.nucleus mustEqual Point(4,1)
    }
  }


  "collision" should {
    val isCollision = PrivateMethod[Boolean]('isCollision)
    "happen for " + cell1 + " and " + cell1 in {
      board_t1 invokePrivate isCollision(cell1) mustEqual true
    }
    "NOT happen for " + cell1 + " and " + RCell(0,1,3,4) + " and " + cell2 in {
      board_t1 invokePrivate isCollision(RCell(0,1,3,4)) mustEqual false
    }
  }


  "allowable board values (of size " + board_t1.dimensions + ")" should {
    val onBoard = PrivateMethod[Boolean]('onBoard)
    "are not < than 0" in {
      board_t1 invokePrivate onBoard(RCell(-1,-1,2,2)) mustBe false
    }
    "are not >= than the board length" in {
      board_t1 invokePrivate onBoard(VCell(0,board_t1.dimensions+1,3,2)) mustBe false
    }
    "are >= than 0 and < dimensions" in {
      board_t1 invokePrivate onBoard(RCell(0,-0,3,3)) mustBe true
    }
  }


  "valid overlapping moves " should {
    val right = PrivateMethod[Board]('right)
    "of "+ cell1 + "right on " + cell2 +"are valid" in
      { board_t1 invokePrivate right(Point(1,1)) mustEqual
      Success(Board(Team(VCell(1,0,6,3),RCell(1,0,4,3),cell2,cell3,cell4,cell5,cell6, cell7),
                    Team(cell8,cell9,cell10,cell11,cell12,cell13,cell14)))}
  }


  "valid moves for " + cell1 should {
      val isValidCellState = PrivateMethod[Boolean]('isValidCellState)
      "are not move up" in { board_t1 invokePrivate isValidCellState(cell1.up) mustBe false }
      "are not move left" in { board_t1 invokePrivate isValidCellState(cell1.left) mustBe false }
      "are move down" in  { board_t1 invokePrivate isValidCellState(cell1.down)  mustBe true }
      "are move right" in { board_t1 invokePrivate isValidCellState(cell1.right) mustBe true }
    }


  "valid moves for " + cell2 should {
    val isValidCellState = PrivateMethod[Boolean]('isValidCellState)
    "are not move up"    in { board_t1 invokePrivate isValidCellState(cell2.up) mustBe false }
    "are not move left"  in { board_t1 invokePrivate isValidCellState(cell2.left) mustBe true }
    "are move down"  in { board_t1 invokePrivate isValidCellState(cell2.down) mustBe true }
    "are move right" in { board_t1 invokePrivate isValidCellState(cell2.right) mustBe true }
  }

  "inner cells of" should {
    cell1 + " are "  in {
      cell1.innerPoints.toSet mustEqual List(Point(1,1),Point(2,1), Point(1,2), Point(2,2)).toSet
    }
    VCell(3,3,9,9) + " are " + VCell(4,4,8,8) in {
      VCell(3,3,9,9).innerPoints mustEqual VCell(4,4,8,8).allPoints
    }
    VCell(9,9,3,3) + " are " + VCell(8,8,4,4) in {
      VCell(9,9,3,3).innerPoints mustEqual VCell(8,8,4,4).allPoints
    }
  }


  "inner cells of" should {
    VCell(3,3,9,9) + " are not " + VCell(4,4,8,9) in {
      VCell(3,3,9,9).innerPoints == VCell(4,4,8,9).allPoints mustBe false
    }
    VCell(0,0,9,4) + " are not " + VCell(1,1,9,4) in {
      VCell(0,0,9,4).innerPoints == VCell(1,1,9,4).allPoints mustBe false
    }
  }


  cell3 + " doesn't contain" should {
    "" + RCell(0,0,3,3) in {
      cell3 contains RCell(0,0,3,3) mustBe false
    }
    "" + RCell(3,0,6,3) in {
      cell3 contains RCell(3,0,6,3) mustBe false
    }
    "" + RCell(6,0,9,3) in {
      cell3 contains RCell(6,0,9,3) mustBe false
    }
    "" + RCell(6,3,9,6) in {
      cell3 contains RCell(6,3,9,6) mustBe false
    }
    "" + RCell(0,6,3,9) in {
      cell3 contains RCell(0,6,3,9) mustBe false
    }
    "" + RCell(3,6,6,9) in {
      cell3 contains RCell(3,6,6,9) mustBe false
    }
    "" + RCell(6,6,9,9) in {
      cell3 contains RCell(6,6,9,9) mustBe false
    }
  }


  cell3 + "does contain " should {
    "" + RCell(1,1,4,4) in {
      cell3 contains RCell(1,1,4,4) mustBe true
    }
    "and in reverse 1" in {
      RCell(1,1,4,4) contains cell3 mustBe true
    }
    "" + RCell(2,1,5,4) in {
      cell3 contains RCell(2,1,5,4) mustBe true
    }
    "and in reverse 2" in {
      RCell(2,1,5,4) contains cell3 mustBe true
    }
    "" + VCell(3,3,9,9) in {
      cell3 contains VCell(3,3,9,9) mustBe true
    }
    "and in reverse 3" in {
      VCell(3,3,9,9) contains cell3 mustBe true
    }
    "" + RCell(3,4,6,7) in {
      cell3 contains RCell(3,4,6,7) mustBe true
    }
    "and in reverse 5" in {
      RCell(3,4,6,7) contains  cell3 mustBe true
    }
  }


  "merging" should {
    cell1 + "merge " + RCell(0,1,3,4) + " is " + VCell(0,0,3,4) in {
      cell1 merge RCell(0,1,3,4)  mustBe VCell(0,0,3,4) }

    cell1 + " merge " + RCell(2,2,5,5) + " is " + VCell(0,0,5,5) in {
      cell1 merge RCell(2,2,5,5) mustEqual VCell(0,0,5,5)  }

    cell1  + " merge " + RCell(2,0,5,3) + " merge " + RCell(4,1,7,4) + " is " + VCell(0,0,7,4) in {
      cell1 merge RCell(2,0,5,3) merge RCell(4,1,7,4) mustEqual VCell(0,0,7,4)  }

  }


  "replaceHead" should {
    val replaceHead = PrivateMethod[List[Cell]]('replaceHead)
    " r r xs" in {
      board_t1 invokePrivate replaceHead(team9.cells) mustEqual team9.cells
    }
    "v r xs" in {
      board_t1 invokePrivate replaceHead(team10.cells) mustEqual team10.cells
    }
    "v v xs" in {
      board_t1 invokePrivate replaceHead(team11.cells) mustEqual VCell(0,0,4,4) :: team1.cells
    }
  }

  "getTeamHead" should {
    val getTeamHead = PrivateMethod[List[Cell]]('getTeamHead)
    "should give None for time 1" in {
      board_t1 invokePrivate getTeamHead(board_t1, 1) mustEqual None
    }
    "should give None for time 2" in {
      board_t2 invokePrivate getTeamHead(board_t2, 1) mustEqual None
    }
  }


  "recMerge" should {
    "at time 1" in {
      board_t1 recMerge(board_t1, 1) mustEqual board_t1
    }
    "at time 2" in {
      board_t2 recMerge(board_t2, 1) mustEqual Board(Team(VCell(2,3,8,10) :: team3.cells), team4)
    }
  }


  cell3 + " moved down is" should {
    "" + RCell(2,4,5,7) in {
      board_t1.down(Point(3,4)) mustEqual
        Success(Board(Team(RCell(2,4,5,7),cell1,cell2,cell4,cell5,cell6, cell7),
                      Team(cell8, cell9,cell10,cell11,cell12,cell13,cell14)))
    }
  }


  val cell15 = RCell(2,2,5,5)
  "" + cell3 + " moved up is" should {
    "" + cell15 in {
      board_t1.up(Point(3,4)) mustEqual
        Success(Board(Team(VCell(0,0,6,5),cell15,cell1,cell2,cell4,cell5,cell6,cell7),
                      Team(cell8,cell9,cell10,cell11,cell12,cell13,cell14)))
    }
  }


  val cell16 = RCell(1,3,4,6)
  cell3 + " moved left is" should {
    "" + cell16 in {
      board_t1.left(Point(3,4)) mustEqual
        Success(Board(Team(cell16,cell1,cell2,cell4,cell5,cell6,cell7),
                      Team(cell8,cell9,cell10,cell11,cell12,cell13,cell14)))
    }
  }


  val cell17 = RCell(3,3,6,6)
  cell3 + " moved right is" should {
    "" + cell17 in {
      board_t1.right(Point(3,4)) mustEqual
        Success(Board(Team(VCell(2,3,8,10),cell17,cell1,cell2,cell4,cell5,cell6,cell7),
                      Team(cell8,cell9,cell10,cell11,cell12,cell13,cell14)))
    }
  }

}
