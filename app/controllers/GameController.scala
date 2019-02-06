package controllers

import javax.inject.{Inject, Singleton}

import play.api.mvc.{Action, Controller}
import services.Counter
import com.google.gson.Gson
import game.{Cell, Coordinate, Team}
import model.{Board, BoardWrapper}
import play.api.libs.json.{JsObject, JsValue, Json, Reads}

import scala.util.{Failure, Success}

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

    val boardWrapper = BoardWrapper(board, 0, 0, "down")

    Ok(BoardWrapper.writeBoard(boardWrapper))

  }

  def move() = Action {
    request =>

      val boardWrapper = Json.fromJson[BoardWrapper](request.body.asJson.get).get

      val board = boardWrapper.board

      val movePoint = Coordinate(boardWrapper.x, boardWrapper.y)

      val move = boardWrapper.move

      if (move == "up") {

        board.up(movePoint) match {
          case Success(v) =>
            val boardWrapperResult = BoardWrapper(v, boardWrapper.x, boardWrapper.y, "none")
            Ok(Json.toJson(boardWrapperResult))
          case Failure(e) =>
            Ok("INVALID MOVE: " + e)
        }

      }
      else if (move == "down") {

        board.down(movePoint) match {
          case Success(v) =>
            val boardWrapperResult = BoardWrapper(v, boardWrapper.x, boardWrapper.y, "none")
            Ok(Json.toJson(boardWrapperResult))
          case Failure(e) =>
            Ok("INVALID MOVE: " + e)
        }
      }
      else if (move == "left") {

        board.left(movePoint) match {
          case Success(v) =>
            val boardWrapperResult = BoardWrapper(v, boardWrapper.x, boardWrapper.y, "none")
            Ok(Json.toJson(boardWrapperResult))
          case Failure(e) =>
            Ok("INVALID MOVE: " + e)
        }

      } else {

        board.right(movePoint) match {
          case Success(v) =>
            val boardWrapperResult = BoardWrapper(v, boardWrapper.x, boardWrapper.y, "none")
            Ok(Json.toJson(boardWrapperResult))
          case Failure(e) =>
            Ok("INVALID MOVE: " + e)
        }
      }

  }






}
