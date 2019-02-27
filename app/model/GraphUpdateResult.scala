package model

import game.{Cell, RCell}

import scala.collection.mutable.ListBuffer

/**
  * Created by kevin on 27/02/19.
  */
case class GraphUpdateResult(adj: Array[Array[Int]],
                             vCells: ListBuffer[Cell],
                             mergedAlready: ListBuffer[ListBuffer[RCell]]) {

}
