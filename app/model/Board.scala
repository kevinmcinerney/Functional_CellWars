package model

import java.io.PrintStream

import game.{Cell, Point, RCell}

import scala.Console._
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}
import Console.{BLACK_B, BLINK, BLUE_B, MAGENTA_B, RED_B, RESET}

/**
  * Created by kevin on 29/01/19.
  */

class BadMoveException(msg: String) extends Exception(msg)

case class Board(rCells: ListBuffer[RCell], vCells: ListBuffer[Cell], edges: Array[Array[Int]]){

  def dimensions: Int = { rCells.length * 2 }

  def up(point: Point): Try[Board] = validateMove(_.up, point) match {
    case Success(i) => Success(move(_.up,i))
    case Failure(m) => Failure(m)
  }

  def down(point: Point): Try[Board] = validateMove(_.down, point) match {
    case Success(i) => Success(move(_.down,i))
    case Failure(m) => Failure(m)
  }

  def left(point: Point): Try[Board] = validateMove(_.left, point) match {
    case Success(i) => Success(move(_.left, i))
    case Failure(m) => Failure(m)
  }

  def right(point: Point): Try[Board] = validateMove(_.right, point) match {
    case Success(i) => Success(move(_.right, i))
    case Failure(m) => Failure(m)
  }

  private def moveCell(fx: RCell => RCell, idx: Int): ListBuffer[RCell] = {

    // Make Copy because is immutable
    val rCellsCopy = rCells.clone()

    // Make move
    rCellsCopy(idx) = fx(rCellsCopy(idx))

    rCellsCopy
  }

  private def move(fx: RCell => RCell, idx: Int): Board = {

    // Move Real Cell
    val rCellsCopy = moveCell(fx, idx)

    // Pattern match graph
    Graph(Board(rCellsCopy, vCells, edges)).update(idx)  match {

        // Merges Needed
      case Some(GraphUpdateResult(newEdges, connected, unconnected)) =>
        merge(newEdges, connected, unconnected, rCellsCopy, idx)
        // No merges needed
      case None => Board(rCellsCopy, vCells, edges)
    }
  }

  private def merge(edges: Array[Array[Int]],
            connected: ListBuffer[Cell],
            unconnected: ListBuffer[RCell],
            rCellsCopy: ListBuffer[RCell],
            idx: Int): Board = {

    // Get Virtual Cells
    val vrMerged = rec(connected, unconnected, rCellsCopy, idx)

    // Capture Real Cells
    val capturedRCells = capture(rCellsCopy, vrMerged, idx)

    Board(capturedRCells, vrMerged, edges)
  }

  private def rec(connected: ListBuffer[Cell], unconnected: ListBuffer[RCell], rCellsCopy: ListBuffer[RCell], idx: Int): ListBuffer[Cell] = {

    // Recursively merge V-V until no change
    val redVCells = recMergeVirtualCells(connected)

    // Recursively merge V-R until no change
    val vrMerged = recMergeVirtualAndReal(rCellsCopy(idx), redVCells, unconnected.asInstanceOf[ListBuffer[Cell]])

    // Recursively call self until no change
    if(connected == vrMerged) connected else rec(vrMerged, unconnected, rCellsCopy, idx)
  }

  private def capture(rCells: ListBuffer[RCell], vCells: ListBuffer[Cell], idx: Int): ListBuffer[RCell] = {
    val rCopy = rCells.clone()
    if(vCells.nonEmpty){
      rCopy.foreach(r => if (vCells.head contains r) {
        val ix = rCopy.indexOf(r)
        rCopy(ix) = r.capture(rCopy(idx).marker).asInstanceOf[RCell]
      })
    }
    rCopy
  }

  private def recMergeVirtualCells(vc: ListBuffer[Cell]): ListBuffer[Cell] = {

    val pairs = vc.combinations(2).to[ListBuffer]

    val merges = pairs.filter(p => p.head contains p.tail.head)

    if(merges.nonEmpty){
      val newVCells = merges.map(pair => pair.head merge pair.tail.head)
      merges.foreach(m => {vc -= m.head; vc -= m.tail.head} )
      recMergeVirtualCells(newVCells ++ vc)
    }
    else { vc }
  }

  private def recMergeVirtualAndReal(mCell: RCell, vc: ListBuffer[Cell], oc: ListBuffer[Cell] ): ListBuffer[Cell] = {
    val pairs = vc.flatMap(v => oc.map(o => (v, o)))
    val newVCells: ListBuffer[Cell] =
    for((left, right) <- pairs if left contains right) yield {
      oc -= (left,right)
      captureVCells(left,right,mCell)
    }
    if(newVCells.nonEmpty) recMergeVirtualAndReal(mCell, recMergeVirtualCells(newVCells ++ vc), oc)
    else vc
  }

  private def captureVCells(left: Cell, right: Cell, mCell: Cell): Cell = {
    val m = left merge right
    if(m contains mCell){
      m.capture(mCell.marker)
    }else{ m }
  }

  private def validateMove(fx: RCell => RCell, nucleus: Point): Try[Int] = {
    val mover_idx = rCells.indexWhere(_.nucleus == nucleus)
    val mover = fx(rCells(mover_idx).copy())
    if(mover_idx != -1)
      if(isValidCellState(mover))
        Success(mover_idx)
      else
        Failure(new BadMoveException("Invalid move from this position: " + rCells(mover_idx) + " " + dimensions))
    else
      Failure(new BadMoveException("You didn't select a cell to move"))
  }

  private def isValidCellState(cell: RCell): Boolean = {
    onBoard(cell) && !isCollision(cell)
  }

  private def onBoard(cell: Cell): Boolean = {
    val inRange = (i: Int) => i >= 0 && i <= dimensions
    inRange(cell.x1) && inRange(cell.x2) &&
      inRange(cell.y1) && inRange(cell.y2)
  }

  private def isCollision(cell: RCell): Boolean = {
    rCells.exists(_.nucleus == cell.nucleus)
  }

  def to2DArray: Array[Array[Int]] = {

    val matrix = Array.fill[Int](dimensions,dimensions) { 0 }

    def addReal(cells: ListBuffer[RCell]): Array[Array[Int]] ={
      for{ c <- cells
           p <- c.drawPoints
      } matrix(p.y)(p.x) = c.marker
      matrix
    }

    def addVirtual(cells: ListBuffer[Cell]): Array[Array[Int]] ={
      for{ c <- cells
           p <- c.drawPoints
      } matrix(p.y)(p.x) = c.marker
      matrix
    }

    def addNucleus(cells: ListBuffer[RCell]): Array[Array[Int]] = {
      for{ c <- cells
      } matrix(c.nucleus.y)(c.nucleus.x) = c.marker * 3
      matrix

    }

    addReal(rCells)

    addVirtual(vCells)

    addNucleus(rCells)

  }

  def print: Unit = {
    for(i <- this.to2DArray.indices){
      for(j <- this.to2DArray.indices){
             if (to2DArray(i)(j) == 1) printf(s"${MAGENTA_B} *${RESET}")
        else if (to2DArray(i)(j) == 3) printf("%02d".format(rCells.indexWhere(_.nucleus == Point(j,i))).toString)
        else if (to2DArray(i)(j) == 2) printf(s"${BLUE_B} *${RESET}")
        else if (to2DArray(i)(j) == 6) printf(s"${RED_B}%02d".format(rCells.indexWhere(_.nucleus == Point(j,i))).toString)
        else if (to2DArray(i)(j) == 0) printf(s"${BLACK_B}  ${RESET}")
        else printf("")
      }
      println
    }
  }

  def print(out: PrintStream): Unit = {
    for(i <- this.to2DArray.indices){
      for(j <- this.to2DArray.indices){
        if (to2DArray(i)(j) == 1) out.printf(s"${MAGENTA_B} *${RESET}")
        else if (to2DArray(i)(j) == 3) out.printf("%02d".format(rCells.indexWhere(_.nucleus == Point(j,i))).toString)
        else if (to2DArray(i)(j) == 2) out.printf(s"${BLUE_B} *${RESET}")
        else if (to2DArray(i)(j) == 6) out.printf("%02d".format(rCells.indexWhere(_.nucleus == Point(j,i))).toString)
        else if (to2DArray(i)(j) == 0) out.printf(s"${BLACK_B}  ${RESET}")
        else out.printf("")
      }
      out.println
    }
  }
}

object Board { }
