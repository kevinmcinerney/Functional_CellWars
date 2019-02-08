package model

import scala.collection.mutable.ListBuffer

/**
  * Created by kevin on 06/02/19.
  */
object Database {

  val database: ListBuffer[Board] = ListBuffer()

  def addBoard(board: Board): Unit = {
    database += board
  }

  def getBoard(): Board = {
    database.foreach(println)
    database.remove(0)
  }






}
