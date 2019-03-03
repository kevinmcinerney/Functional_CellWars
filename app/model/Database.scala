package model

import scala.collection.mutable.ListBuffer

/**
  * Created by kevin on 06/02/19.
  */
object Database {

  var database: ListBuffer[Board] = ListBuffer()

  def addBoard(board: Board): Unit = {
    database += board
  }

  def getBoard(): Board = {
    database.remove(0)
  }

  def clearBoard(): Unit = {
    this.database = ListBuffer()
  }






}
