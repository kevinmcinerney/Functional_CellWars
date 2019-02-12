package model

import game.Team

/**
  * Created by kevin on 31/01/19.
  */
case class BoardWrapper(board: Board, x: Int, y: Int, move: String)

import play.api.libs.json._
import play.api.libs.functional.syntax._


object BoardWrapper{

//
//  import play.api.libs.json._
//
//  implicit val boardWrapperFormats = Json.format[BoardWrapper]
//
//  def writeBoard(boardWrapper: BoardWrapper): JsValue = {
//    Json.toJson(boardWrapper)
//  }
//
//  def readBoard(jsonBoardWrapper: JsValue): BoardWrapper = {
//    val board = (jsonBoardWrapper \ "board").as[Board]
//    val x = (jsonBoardWrapper \ "x").as[Int]
//    val y = (jsonBoardWrapper \ "y").as[Int]
//    val move = (jsonBoardWrapper \ "move").as[String]
//    BoardWrapper(board, x, y, move)
//  }
}