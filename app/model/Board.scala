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

    val rCellsCopy = moveCell(fx, idx)

    // Init Graph with moved cell
    val g = Graph(Board(rCellsCopy,vCells,edges))

    // Get new edges, vcells, and list of merged rcells
    g.update(idx)  match {

      case Some(GraphUpdateResult(newEdges, newVCells, alreadyMerged)) => {

        // Merge virtual to virtual
        val redVCells = recMergeVirtualCells(newVCells)

        // Make outer cells
        val outerCells = rCellsCopy.clone() ++ redVCells

        // Remove merged cells from outer cells
        alreadyMerged.flatten.foreach(c => outerCells -= c)

        // Merge virtual to real
        val vrMerged =
        if(redVCells.nonEmpty && redVCells.length < newVCells.length)
          recMergeVirtualAndReal(redVCells, outerCells)
        else if (redVCells.nonEmpty && redVCells.length == newVCells.length)
          recMergeVirtualAndReal(outerCells, redVCells)
        else ListBuffer[Cell]()

        // Replace redVCells with any new vcells from r-v merges
        val redVMerge = vrMerged ++ redVCells.filterNot(_ contains vrMerged.head)

        if(vrMerged.nonEmpty){
          // Capture
          rCellsCopy.foreach(r => if (vrMerged.head contains r) {
            val ix = rCellsCopy.indexOf(r)
            rCellsCopy(ix) = r.capture(rCellsCopy(idx).marker)
          })
        }

        Board(rCellsCopy, redVMerge, newEdges)
      }
        // Only the moved rCell has changed
      case None => { Board(rCellsCopy, vCells, edges) }
    }
  }

  private def recMergeVirtualCells(vc: ListBuffer[Cell]): ListBuffer[Cell] = {
    val pairs = vc.combinations(2).to[ListBuffer]
    val merges = pairs.filter(p => p.head contains p.tail.head)
    if(merges.nonEmpty){
      val newVCells = merges.map(pair => pair.head merge pair.tail.head)
      merges.foreach(m => {newVCells -= m.head; newVCells -= m.tail.head} )
      recMergeVirtualCells(newVCells)
    }
    else { vc }
  }

  private def recMergeVirtualAndReal(vc: ListBuffer[Cell], oc: ListBuffer[Cell] ): ListBuffer[Cell] = {
    val pairs = vc.map(v => oc.map(o => (v, o)))
    val merges = pairs.flatten.filter(p => p._1 contains p._2)
    if(merges.nonEmpty){
      val newVCells = merges.map(pair => pair._1 merge pair._2)
      merges.foreach(m => { oc -= m._2; oc -= m._1 })
      recMergeVirtualAndReal(newVCells, oc)
    }
    else { vc }
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
        else if (to2DArray(i)(j) == 6) printf("%02d".format(rCells.indexWhere(_.nucleus == Point(j,i))).toString)
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
