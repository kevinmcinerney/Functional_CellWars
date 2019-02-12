package controllers

import javax.inject.{Inject, Singleton}

import play.api.mvc.{Action, Controller}
import services.Counter
import com.google.gson.Gson
import game.{Cell, Point, RCell, Team}
import model.{Board, BoardWrapper, Database, Move}
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
      } yield RCell(x, y, x + 3, y + 3)

      Team(team.toList)

    }

    val numPerTeam = size / 4

    val teamOne = loadTeam(0, numPerTeam)

    val teamTwo = loadTeam(size - 3, numPerTeam)

    val board = Board(teamOne, teamTwo)

    Database.addBoard(board)

    val boardWrapper = BoardWrapper(board, 0, 0, "down")

    //Ok(BoardWrapper.writeBoard(boardWrapper))
    Ok("hello")

  }

  def move() = Action {
    request =>

      val board = Database.getBoard()

      val move = Json.fromJson[Move](request.body.asJson.get).get

      val movePoint = Point(move.x, move.y)

      val direction = move.move

      if (direction == "up") {

        board.up(movePoint) match {
          case Success(v) =>
            Database.addBoard(v)
            val boardWrapperResult = BoardWrapper(v, move.x, move.y, "none")
            //Ok(Json.toJson(boardWrapperResult))
            Ok("hello")
          case Failure(e) =>
            Ok("INVALID MOVE: " + e)
        }

      }
      else if (direction == "down") {

        board.down(movePoint) match {
          case Success(v) =>
            Database.addBoard(v)
            val boardWrapperResult = BoardWrapper(v, move.x, move.y, "none")
            //Ok(Json.toJson(boardWrapperResult))
            Ok("hello")
          case Failure(e) =>
            Ok("INVALID MOVE: " + e)
        }
      }
      else if (direction == "left") {

        board.left(movePoint) match {
          case Success(v) =>
            Database.addBoard(v)
            val boardWrapperResult = BoardWrapper(v, move.x, move.y, "none")
            //Ok(Json.toJson(boardWrapperResult))
            Ok("hello")
          case Failure(e) =>
            Ok("INVALID MOVE: " + e)
        }

      } else {

        board.right(movePoint) match {
          case Success(v) =>
            Database.addBoard(v)
            val boardWrapperResult = BoardWrapper(v, move.x, move.y, "none")
            //Ok(Json.toJson(boardWrapperResult))
            Ok("hello")
          case Failure(e) =>
            Ok("INVALID MOVE: " + e)
        }
      }

  }






}
