package game

import breeze.math.CoordinateField

/**
  * Created by kevin on 25/04/17.
  */

class Game(dimensions: Int) {

  val board = Board(this.dimensions)

  def loadTeam(x: Int, teamSize: Int): Team = {

    val team = for {
      y <- 0 until (teamSize * 4) by 4
    } yield Cell(Coordinate(x, y), Coordinate(x + 3, y + 3))

    Team(team.toList)

  }








}
