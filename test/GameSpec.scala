import org.scalatestplus.play.PlaySpec
import game.{Cell, Point, Team}
import model.Board
import org.scalatest.{FlatSpec, PrivateMethodTester}

import scala.util.Success


/**
  * Created by kevin on 25/04/17.
  */
class GameSpec extends PlaySpec with PrivateMethodTester {

  val cell1 = Cell(0,0,2,2)
  val cell2 = Cell(20,20,23,23)
  val cell3 = Cell(7,7,10,10)
  val cell4 = Cell(3,3,6,6)  // merge
  val cell5 = Cell(2,2,5,5)  // merge
  val cell6 = Cell(0,5,3,8)  // merge

  val cell7 = Cell(7,8,10,11)

  val team = Team(cell1,cell2,cell3,cell4,cell5,cell6)
  val team2 = Team(cell7,cell7,cell7,cell7,cell7,cell7)
  val board = Board(team, team2)


  "real cells are like" should {
    ""+cell4 in {
      cell4.realCell mustEqual true
    }
    ""+cell5 in {
      cell5.realCell mustEqual true
    }
    ""+cell6 in {
      cell6.realCell mustEqual true
    }
  }


  "real cells are NOT like" should {
    ""+cell1 in {
      cell1.realCell mustEqual false
    }
  }


  "nucleus is" should {
    Point(8,8) + " in " + cell3 in {
      cell3.nucleus.getOrElse(None) mustEqual Point(8,8)
    }
    Point(4,4) + " in " + cell4 in {
      cell4.nucleus.getOrElse(None) mustEqual Point(4,4)
    }
  }


  "nucleus doesn't exist for" should {
    ""+cell1 in {
      cell1.nucleus.getOrElse(None) mustEqual None
    }
  }


  "collision" should {
    val isCollision = PrivateMethod[Boolean]('isCollision)
    "happens for " + cell5 + " and " + cell5 in {
      board invokePrivate isCollision(cell5) mustEqual true
    }
    "doesn't happens for " + Cell(0,0,3,3) + " and " + cell1 in {
      board invokePrivate isCollision(Cell(0,0,3,3)) mustEqual false
    }
    "doesn't happens for " + cell1 + " and " + cell1 in {
      board invokePrivate isCollision(Cell(0,0,2,2)) mustEqual false
    }
    "doesn't happens for " + Cell(4,4,7,7) + " and " + cell4 in {
      board invokePrivate isCollision(Cell(4,4,7,7)) mustEqual false
    }
  }


  "allowable board values (of size " + board.dimensions + ")" should {
    val onBoard = PrivateMethod[Boolean]('onBoard)
    "are not < than 0" in {
      board invokePrivate onBoard(Cell(-1,-1,2,2)) mustBe false
    }
    "are not >= than the board length" in {
      board invokePrivate onBoard(Cell(0,board.dimensions+1,2,2)) mustBe false
    }
    "are >= than 0 and < dimensions" in {
      board invokePrivate onBoard(Cell(0,-0,3,3)) mustBe true
    }
  }


  "valid overlapping moves " should {
    val teamOne = Team(Cell(0,1,3,4), Cell(0,4,3,7), cell2, cell3, cell2, cell3)
    val teamTwo = Team.nullTeam
    val board = Board(teamOne, teamTwo)
    val down = PrivateMethod[Board]('down)
    "of "+ Cell(0,1,3,4) + "down on " + Cell(0,4,3,7) +"are valid" in
      { board invokePrivate down(Point(1,2)) mustEqual
      Success(Board(Team(Cell(0,2,3,5), Cell(0,4,3,7),cell2,cell3,cell2,cell3),
                    Team.nullTeam,
                    Team(Cell(0,2,3,7))))}
  }


  "valid moves for " + cell1 should {
      val isValidCellState = PrivateMethod[Boolean]('isValidCellState)
      "are not move up" in { board invokePrivate isValidCellState(cell1.up) mustBe false }
      "are not move left" in { board invokePrivate isValidCellState(cell1.left) mustBe false }
      "are move down" in  { board invokePrivate isValidCellState(cell1.down)  mustBe true }
      "are move right" in { board invokePrivate isValidCellState(cell1.right) mustBe true }
    }


  "valid moves for " + cell2 should {
    println(board.dimensions)
    val isValidCellState = PrivateMethod[Boolean]('isValidCellState)
    "are not move up"    in { board invokePrivate isValidCellState(cell2.up) mustBe true }
    "are not move left"  in { board invokePrivate isValidCellState(cell2.left) mustBe true }
    "are move down"  in { board invokePrivate isValidCellState(cell2.down) mustBe false }
    "are move right" in { board invokePrivate isValidCellState(cell2.right) mustBe false }
  }


  "valid moves for " + Cell(2,3,5,6) should {
    val isValidCellState = PrivateMethod[Boolean]('isValidCellState)
    "are not move right"    in { board invokePrivate isValidCellState(Cell(2,3,5,6).right) mustBe false }
    "are not move left"  in { board invokePrivate isValidCellState(Cell(2,3,5,6).left) mustBe true }
    "are move down"  in { board invokePrivate isValidCellState(Cell(2,3,5,6).down) mustBe true }
    "are move up" in { board invokePrivate isValidCellState(Cell(2,3,5,6).up) mustBe false }
  }


  "inner cells of" should {
    cell4 + " are " + Cell(4,4,5,5) in {
      Cell(3,3,6,6).innerCells mustEqual Cell(4,4,5,5).outerCells
    }
    Cell(3,3,9,9) + " are " + Cell(4,4,8,8) in {
      Cell(3,3,9,9).innerCells mustEqual Cell(4,4,8,8).outerCells
    }
    Cell(0,0,9,3) + " are " + Cell(1,1,8,2) in {
      Cell(0,0,9,3).innerCells mustEqual Cell(1,1,8,2).outerCells
    }
  }


  "inner cells of" should {
    cell4 + " are not " in {
      Cell(3,3,6,6).innerCells == Cell(4,4,6,6).outerCells mustBe false
    }
    Cell(3,3,9,9) + " are not " + Cell(4,4,8,9) in {
      Cell(3,3,9,9).innerCells == Cell(4,4,8,9).outerCells mustBe false
    }
    Cell(0,0,9,3) + " are not " + Cell(1,1,9,2) in {
      Cell(0,0,9,3).innerCells == Cell(1,1,9,2).outerCells mustBe false
    }
  }


  cell4 + " doesn't contain" should {
    "" + Cell(0,0,3,3) in {
      cell4 contains Cell(0,0,3,3) mustBe false
    }
    "" + Cell(3,0,6,3) in {
      cell4 contains Cell(3,0,6,3) mustBe false
    }
    "" + Cell(6,0,9,3) in {
      cell4 contains Cell(6,0,9,3) mustBe false
    }
    "" + Cell(0,3,3,6) in {
      cell4 contains Cell(0,3,3,6) mustBe false
    }
    "" + Cell(6,3,9,6) in {
      cell4 contains Cell(6,3,9,6) mustBe false
    }
    "" + Cell(0,6,3,9) in {
      cell4 contains Cell(0,6,3,9) mustBe false
    }
    "" + Cell(3,6,6,9) in {
      cell4 contains Cell(3,6,6,9) mustBe false
    }
    "" + Cell(6,6,9,9) in {
      cell4 contains Cell(6,6,9,9) mustBe false
    }
  }


  cell4 + "does contain " should {
    "" + Cell(1,1,4,4) in {
      cell4 contains Cell(1,1,4,4) mustBe true
    }
    "and in reverse 1" in {
      Cell(1,1,4,4) contains cell4 mustBe true
    }
    "" + Cell(2,1,5,4) in {
      cell4 contains Cell(2,1,5,4) mustBe true
    }
    "and in reverse 2" in {
      Cell(2,1,5,4) contains cell4 mustBe true
    }
    "" + Cell(3,3,9,9) in {
      cell4 contains Cell(3,3,9,9) mustBe true
    }
    "and in reverse 3" in {
      Cell(3,3,9,9) contains cell4 mustBe true
    }
    "" + Cell(5,5,8,8) in {
      cell4 contains Cell(5,5,8,8) mustBe true
    }
    "and in reverse 4" in {
      Cell(5,5,8,8) contains cell4 mustBe true
    }
    "" + Cell(3,4,6,7) in {
      cell4 contains Cell(3,4,6,7) mustBe true
    }
    "and in reverse 5" in {
     Cell(3,4,6,7) contains  cell4 mustBe true
    }
  }


  "merging" should {
    cell4 + "merge " + Cell(0,0,6,6) + " is None" in {
      cell4 merge Cell(0,0,6,6)  mustBe  Cell(0,0,6,6) }

    cell4 + " merge " + cell5 + " is " + Cell(2,2,6,6) in {
      cell4 merge cell5 mustEqual Cell(2,2,6,6)  }

    cell4  + " merge " + cell5 + " is " + Cell(2,0,6,6) in {
      cell4 merge Cell(2,0,5,3) mustEqual Cell(2,0,6,6)  }

    cell4 + " merge " + cell5 + " merge " + Cell(4,0,7,3) + " is " + Cell(2,0,7,6) in {
      cell4 merge cell5 merge Cell(4,0,7,3) mustEqual Cell(2,0,7,6)
    }
  }


  "merging teams" should {
    val merge = PrivateMethod[List[Cell]]('merge)
    "should give correct new team" in {
      board invokePrivate merge(board) mustEqual  List(Cell(7,7,10,11),Cell(2,2,6,6))
    }
  }


  "recursively merging teams" should {
    "should give correct new team" in {
      board.recMerge(board) mustEqual Board(team, team2, Team(Cell(7,7,10,11),Cell(0,2,6,8)))
    }
  }


  cell3 + " moved down is" should {
    "" + cell7 in {
      Board(team, Team.nullTeam).down(Point(8,8)) mustEqual
        Success(Board(Team(cell7,cell1,cell2,cell4,cell5,cell6),
                      Team.nullTeam,
                      Team(Cell(0,2,6,8))))
    }
  }


  val cell8 = Cell(7,6,10,9)
  "" + cell3 + " moved up is" should {
    "" + cell8 in {
      Board(Team(cell1,cell2,cell3,cell4,cell5,cell6), Team.nullTeam).up(Point(8,8)) mustEqual
        Success(Board(Team(cell8,cell1,cell2,cell4,cell5,cell6),
                      Team.nullTeam,
                      Team(Cell(0,2,6,8))))
    }
  }


  val cell9 = Cell(6,7,9,10)
  cell3 + " moved left is" should {
    "" + cell9 in {
      Board(Team(cell1,cell2,cell3,cell4,cell5,cell6), Team.nullTeam).left(Point(8,8)) mustEqual
        Success(Board(Team(cell9,cell1,cell2,cell4,cell5,cell6),
                      Team.nullTeam,
                      Team(Cell(0,2,6,8))))
    }
  }


  val cell10 = Cell(8,7,11,10)
  cell3 + " moved right is" should {
    "" + cell10 in {
      Board(Team(cell1,cell2,cell3,cell4,cell5,cell6), Team.nullTeam).right(Point(8,8)) mustEqual
        Success(Board(Team(cell10,cell1,cell2,cell4,cell5,cell6),
                      Team.nullTeam,
                      Team(Cell(0,2,6,8))))
    }
  }

}
