package controllers

import javax.inject.{Inject, Singleton}

import play.api.mvc.{Action, Controller}
import services.Counter
import com.google.gson.Gson
import game.{Cell, Coordinate, Team}
import model.{Board, BoardWrapper}
import play.api.libs.json.{JsObject, JsValue, Json, Reads}

/**
  * Created by kevin on 25/04/17.
  */
@Singleton
class GameController  @Inject() extends Controller {

  /**
   fillllllll
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

    val board = Board(teamOne, teamTwo)

    val boardWrapper = BoardWrapper(board, 0,0,"down")

    Ok(BoardWrapper.writeBoard(boardWrapper))

  }

  def move() = Action {
    request =>

      val board = Json.fromJson[BoardWrapper](request.body.asJson.get).get

      val teamOne = board.board.teamOne

      val teamTwo = board.board.teamTwo

      val movePoint = Coordinate(board.x, board.y)

      val move = board.move


      if (move == "up") {
        val t1 = Team.up(teamOne, movePoint)
        //val t2 = Team.up(teamTwo, movePoint)
        val board = Board(t1, teamTwo)
        val boardWrapper = BoardWrapper(board,0,0,"none")
        Ok(Json.toJson(boardWrapper))
      }
      else if (move == "down") {
        val t1 = Team.down(teamOne, movePoint)
        //val t2 = Team.down(teamTwo, movePoint) //fix this which team is picked
        val board = Board(t1, teamTwo)
        val boardWrapper = BoardWrapper(board,0,0,"none")
        Ok(Json.toJson(boardWrapper))
      }
      else if (move == "left") {
        val t1 = Team.left(teamOne, movePoint)
        val t2 = Team.left(teamTwo, movePoint)
        val board = Board(t1, teamTwo)
        val boardWrapper = BoardWrapper(board,0,0,"none")
        Ok(Json.toJson(boardWrapper))

      } else {
        val t1 = Team.right(teamOne, movePoint)
        val t2 = Team.right(teamTwo, movePoint)
        val board = Board(t1, teamTwo)
        val boardWrapper = BoardWrapper(board,0,0,"none")
        Ok(Json.toJson(boardWrapper))
      }

  }






}
