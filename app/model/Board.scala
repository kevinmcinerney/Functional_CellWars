package model

import com.google.gson.Gson
import game.{Cell, Coordinate, Team}

/**
  * Created by kevin on 29/01/19.
  */

//case class WrapperObject(rows: Array[MyJsonObject])
//
//case class MyJsonObject(cols: Array[Grid])

case class Board(dimensions: Int) extends BoardTrait {

  def onBoard(i: Int): Boolean = i >= 0 && i < dimensions

  def isValidMove(cell: Cell): Boolean = {
    onBoard(cell.topLeft.x) && onBoard(cell.botRight.x) &&
      onBoard(cell.topLeft.y) && onBoard(cell.botRight.y)
  }

}
