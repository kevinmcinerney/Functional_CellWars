package model

import game.Cell

/**
  * Created by kevin on 29/04/17.
  */
trait BoardTrait {

  def onBoard(i: Int): Boolean

  def isValidMove(cell: Cell): Boolean

}
