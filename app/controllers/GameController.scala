package controllers

import javax.inject.{Inject, Singleton}

import model.{Grid}
import play.api.mvc.{Action, Controller}
import services.Counter
import com.google.gson.Gson
import game.{Cell, Coordinate, Team}

/**
  * Created by kevin on 25/04/17.
  */
@Singleton
class GameController  @Inject() extends Controller {

  /**
    * Create an action that responds with the [[Counter]]'s current
    * count. The result is plain text. This `Action` is mapped to
    * `GET /count` requests by an entry in the `routes` config file.
    */
  def game = Action { Ok(views.html.game("This is your game")) }


  def startPositions(size: Int) = Action {

    def loadTeam(x: Int, teamSize: Int): Team = {

      val team = for {
        y <- 0 until (teamSize * 4) by 4
      } yield Cell(Coordinate(x, y), Coordinate(x + 3, y + 3))

      Team(team.toList)

    }

    val numPerTeam = size / 4

    val teamOne = loadTeam(0, numPerTeam)

    val teamTwo = loadTeam(size - 3, numPerTeam)

    val array = Array.ofDim[Grid](size, size)
    val length = 600 / size
    for (x_px <- 0 until 600 by length) {
      for (y_px <- 0 until 600 by length) {
        val x = x_px / length
        val y = y_px / length
        array(x)(y) = Grid(length,length, 0, x_px, y_px)
      }
    }

    teamOne.cells.foreach(cell => cell.drawCells
      .foreach(point => {array(point.x)(point.y)
        = Grid(length, length, 1, point.x*length, point.y*length)}))

    teamTwo.cells.foreach(cell => cell.drawCells
      .foreach(point => {array(point.x)(point.y)
        = Grid(length, length, 2, point.x*length, point.y*length)}))

    teamOne.cells.foreach(cell =>  {array(cell.centerCell.x)(cell.centerCell.y)
      = Grid(length, length, 3, cell.centerCell.x*length, cell.centerCell.y*length)})

    teamTwo.cells.foreach(cell =>  {array(cell.centerCell.x)(cell.centerCell.y)
      = Grid(length, length, 4, cell.centerCell.x*length, cell.centerCell.y*length)})


    val gson = new Gson
    val board = gson.toJson(array)

    Ok(board.toString)

  }

  def move() = Action {
    request =>
      val gson = new Gson
      val json = request.body.asJson.get
      val board = gson.fromJson(json.toString(), classOf[Array[Array[Grid]]])

    // CONVERT WRAPPER OBJECT INTO TEAMS -> CELLS -> COORDINATES

    // MOVE CORRECT CELL

    //val jsonBoard = gson.toJson(board)

    Ok(board.toString)

  }


}
