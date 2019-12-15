package controllers

import javax.inject.{Inject, Singleton}

import play.api.mvc.{Action, Controller}

import com.google.gson.Gson
import game.{ Point, RCell}
import model.{Board, Database, Move}
import play.api.libs.json.{Json}

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success}

/**
  * Created by kevin on 25/04/17.
  */
@Singleton
class GameController  @Inject() extends Controller {

  def startPositions(size: Int) = Action {

    Database.clearBoard()

    def loadCells(x: Int, teamSize: Int, marker: Int): ListBuffer[RCell] = {

      for {
        y <- 0 until (teamSize * 4) by 4
      } yield RCell(x, y, x + 3, y + 3, marker)

    }.to[ListBuffer]

    val numPerTeam = size / 4

    val teamOne = loadCells(0, numPerTeam, 1)

    //teamOne.update(0, RCell(34,2,37,5,1))

    val teamTwo = loadCells(size - 3, numPerTeam, 2)

    val rCells = teamOne ++ teamTwo

    val emptyAdj = Array.fill[Array[Int]](rCells.length)(Array.fill[Int](rCells.length)(0))

//    val board = Board(rCells, ListBuffer(), emptyAdj)
//
//    Database.addBoard(board)
//
//    val g = new Gson()
//
//    Ok(g.toJson(board.to2DArray))
    Ok("ok")
  }

  def move() = Action {
    request =>

      val board = Database.getBoard()

      val move = Json.fromJson[Move](request.body.asJson.get).get

      val movePoint = Point(move.x, move.y)

      val direction = move.move

      val g = new Gson()

      if (direction == "up") {

        board.up(movePoint) match {
          case Success(v) =>
            Database.addBoard(v)
            Ok(g.toJson(v.to2DArray))
          case Failure(e) =>
            Database.addBoard(board)
            Ok("INVALID MOVE: " + e)
        }

      }
      else if (direction == "down") {

        board.down(movePoint) match {
          case Success(v) =>
            Database.addBoard(v)
            Ok(g.toJson(v.to2DArray))
          case Failure(e) =>
            Database.addBoard(board)
            Ok("INVALID MOVE: " + e)
        }
      }
      else if (direction == "left") {

        board.left(movePoint) match {
          case Success(v) =>
            Database.addBoard(v)
            Ok(g.toJson(v.to2DArray))
          case Failure(e) =>
            Database.addBoard(board)
            Ok("INVALID MOVE: " + e)
        }

      } else {

        board.right(movePoint) match {
          case Success(v) =>
            Database.addBoard(v)
            Ok(g.toJson(v.to2DArray))
          case Failure(e) =>
            Database.addBoard(board)
            Ok("INVALID MOVE: " + e)
        }
      }

  }
}
