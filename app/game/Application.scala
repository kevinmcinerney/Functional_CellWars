package game
import controllers.GameController
import model.Board

/**
  * Created by kevin on 26/01/19.
  */
object Application extends App{

  def loadTeam(x: Int, teamSize: Int): Team = {

    val team = for {
      y <- 0 until (teamSize * 4) by 4
    } yield Cell(Coordinate(x, y), Coordinate(x + 3, y + 3))

    Team(team.toList)

  }

  val numPerTeam = 40 / 4

  val teamOne = loadTeam(0, numPerTeam)

  val teamTwo = loadTeam(40 - 3, numPerTeam)

  val board = Board(teamOne, teamTwo)


  val movePoint = Coordinate(1, 1)


  val t1 = board.down(movePoint)


}
