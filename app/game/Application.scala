package game
import controllers.GameController
import model.{Board, Database}

/**
  * Created by kevin on 26/01/19.
  */
object Application extends App{

//  def loadTeam(x: Int, teamSize: Int): Team = {
//
//    val team = for {
//      y <- 0 until (teamSize * 4) by 4
//    } yield Cell(Coordinate(x, y), Coordinate(x + 3, y + 3))
//
//    Team(team.toList)
//
//  }
//
//  val numPerTeam = 40 / 4
//
//  val teamOne = loadTeam(0, numPerTeam)
//
//  val teamTwo = loadTeam(40 - 3, numPerTeam)
//
//  val board = Board(teamOne, teamTwo, Team(List()))
//
//
//  val movePoint = Coordinate(1, 1)
//
//
//  val t1 = board.down(movePoint)
//
//  Database.addBoard(board)
//
//  Database.getBoard()
//
//  val b2 = Board(teamTwo, teamOne, Team(List()))
//
//  Database.addBoard(b2)
//
//  println()
//
//  Database.getBoard()


//  val cell1 = Cell(Point(0,0),Point(2,2))
//  val cell2 = Cell(Point(17,17),Point(19,19))
//  val cell3 = Cell(Point(7,7), Point(11,11))
//  val cell4 = Cell(Point(3,3), Point(6,6))
//  val cell5 = Cell(Point(2,2), Point(5,5))
//  val cell6 = Cell(Point(0,5), Point(3,8))
//
//  val list = List(cell1,cell2,cell3,cell4,cell5,cell6)
//
//  private def reduce(list: List[Cell]): Cell = {
//    list
//      .foldLeft(Cell(Point(0,0), Point(0,0)))
//      { (c1: Cell, c2: Cell) => c1 compare c2 }
//  }
//
//   println(reduce(list))

}
