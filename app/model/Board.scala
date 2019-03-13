package model

import java.io.PrintStream

import game.{Cell, Point, RCell, RandomGame}

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Random, Success, Try}
import Console.{BLACK_B, BLUE_B, MAGENTA_B, RED_B, RESET}


/**
  * Represents a state of the game
  */
trait BoardTrait {

  /**
    * Do a random play.
    * @return the final score
    */
  def playout(board: Board): Int

  /**
    * @return all possible game state
    */
  def getAllPossibleNextBoard(player: Int): Seq[Board]
}

/**
  * Exception for invalid moves
  * @param msg message
  */
class BadMoveException(msg: String) extends Exception(msg)


/**
  * The Board representation.
  * @param rCells the RCells on the board
  * @param vCells the VCells on the board
  * @param edges  the graph of connections between rCells
  */
case class Board(rCells: ListBuffer[RCell], vCells: ListBuffer[Cell], edges: Array[Array[Int]])  extends BoardTrait {


  /**
    * Dimensions of square board
    * @return dimensions
    */
  def dimensions: Int = { rCells.length * 2 }


  /**
    * Move Cell up
    * @param point the nucleus of the cell to be moved
    * @return New board for valid move,
    *         Same board for invalid move, (to assist random games)
    *         Exception if point doesn't match any RCell
    */
  def up(point: Point): Try[Board] = validateMove(_.up, point) match {
    case Success(-1) => Success(this) // For random games
    case Success(i) => Success(move(_.up, i))
    case Failure(m) => Failure(m)
  }


  /**
    * Move Cell down
    * @param point the nucleus of the cell to be moved
    * @return New board for valid move,
    *         Same board for invalid move, (to assist random games)
    *         Exception if point doesn't match any RCell
    */
  def down(point: Point): Try[Board] = validateMove(_.down, point) match {
    case Success(-1) => Success(this) // For random games
    case Success(i) => Success(move(_.down, i))
    case Failure(m) => Failure(m)
  }


  /**
    * Move Cell left
    * @param point the nucleus of the cell to be moved
    * @return New board for valid move,
    *         Same board for invalid move, (to assist random games)
    *         Exception if point doesn't match any RCell
    */
  def left(point: Point): Try[Board] = validateMove(_.left, point) match {
    case Success(-1) => Success(this) // For random games
    case Success(i) => Success(move(_.left, i))
    case Failure(m) => Failure(m)
  }


  /**
    * Move Cell right
    * @param point the nucleus of the cell to be moved
    * @return New board for valid move,
    *         Same board for invalid move, (to assist random games)
    *         Exception if point doesn't match any RCell
    */
  def right(point: Point): Try[Board] = validateMove(_.right, point) match {
    case Success(-1) => Success(this) // For random games
    case Success(i) => Success(move(_.right, i))
    case Failure(m) => Failure(m)
  }


  /**
    * Move Cell using fx in copy of [[Board.rCells]]
    * @param fx function to move cell up, down, left or right
    * @param idx index of cell to be moved in [[Board.rCells]]
    * @return new copy of [[Board.rCells]] with moved RCell
    */
  private def moveCell(fx: RCell => RCell, idx: Int): ListBuffer[RCell] = {

    // Make Copy because is immutable
    val rCellsCopy = rCells.clone()

    // Make move
    rCellsCopy(idx) = fx(rCellsCopy(idx))

    rCellsCopy
  }


  /**
    * Move Cell using fx on copy of [[Board.rCells]]
    * @param fx function to move cell up, down, left or right
    * @param idx index of cell to be moved in [[Board.rCells]]
    * @return new Board with moved/merged RCell
    */
  private def move(fx: RCell => RCell, idx: Int): Board = {

    // Move Real Cell
    val rCellsCopy = moveCell(fx, idx)

    // Pattern match graph
    Graph(Board(rCellsCopy, vCells, edges)).update(idx)  match {
        // Merges Needed
      case Some(Board(unconnected, connected, newEdges)) =>
        merge(newEdges, connected, unconnected, rCellsCopy, idx)
        // No merges needed
      case None => Board(rCellsCopy, vCells, edges)
    }
  }


  /**
    * Merge all required VCells/RCells after a move
    * @param edges the graph of connections between RCells
    * @param connected the connected RCells in the graph which can form VCells
    * @param unconnected the unconnected RCells in the graph which cannot form VCells
    * @param rCellsCopy copy of RCells of a board
    * @param idx index of cell to be moved in [[Board.rCells]]
    * @return new Board after moved/merged RCell
    */
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


  /**
    * Recursively 1) Merge virtual-virtual cells and 2) virtual-real cells until stable
    * @param connected the connected RCells in the graph which can form VCells
    * @param unconnected the unconnected RCells in the graph which cannot form VCells
    * @param rCellsCopy copy of RCells of a board
    * @param idx index of cell to be moved in [[Board.rCells]]
    * @return List of new Virtual Cells
    */
  private def rec(connected: ListBuffer[Cell], unconnected: ListBuffer[RCell], rCellsCopy: ListBuffer[RCell], idx: Int): ListBuffer[Cell] = {

    // Recursively merge V-V until no change
    val redVCells = recMergeVirtualCells(connected)

    // Recursively merge V-R until no change
    val vrMerged = recMergeVirtualAndReal(rCellsCopy(idx), redVCells, unconnected.asInstanceOf[ListBuffer[Cell]])

    // Recursively call self until no change
    if(connected == vrMerged) connected else rec(vrMerged, unconnected, rCellsCopy, idx)
  }


  /**
    * Change team of RCells inside of expanded VCells
    * @param rCells the RCells on the board
    * @param vCells the VCells on the board
    * @param idx index of cell to be moved in [[Board.rCells]]
    * @return list of RCells with captured cells assigned to other team
    */
  private def capture(rCells: ListBuffer[RCell], vCells: ListBuffer[Cell], idx: Int): ListBuffer[RCell] = {
    val rCopy = rCells.clone()
    for(i <- rCopy.indices) {
      for (j <- vCells.indices) {
        if (vCells(j) contains rCopy(i)) {
          rCopy(i) = rCopy(i).capture(vCells(j).marker).asInstanceOf[RCell]
        } else {
          rCopy
        }
      }
    }
    rCopy
  }


  /**
    * Recursively self merge virtual cells until stable
    * @param vc the VCells to checked for self-merges
    * @return list of self merged VCells
    */
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


  /**
    * Recursively merge VCells with RCells until stable
    * @param mCell the moved RCell
    * @param vc list of VCells
    * @param oc list of outer Cells (incl. RCells & VCells)
    * @return list of VCells
    */
  private def recMergeVirtualAndReal(mCell: RCell, vc: ListBuffer[Cell], oc: ListBuffer[Cell] ): ListBuffer[Cell] = {
    val pairs = vc.flatMap(v => oc.map(o => (v, o)))
    val newVCells: ListBuffer[Cell] =
    for((left, right) <- pairs if left contains right) yield {
      oc -= (left,right)
      captureVCells(left, right, mCell)
    }
    if(newVCells.nonEmpty) recMergeVirtualAndReal(mCell, recMergeVirtualCells(newVCells ++ vc), oc)
    else vc
  }


  /**
    * Recursively merge VCells with RCells until stable
    * @param left cell to be merged
    * @param right cell to be merged
    * @param mCell the moved RCell whose team will capture the result of merge
    * @return Captured Cell with correct team assigned
    */
  private def captureVCells(left: Cell, right: Cell, mCell: Cell): Cell = {
    val m = left merge right
    if(m contains mCell){
      m.capture(mCell.marker)
    }else{ m }
  }


  /**
    * Determine if move is valid
    * @param fx function to move cell up, down, left or right
    * @param nucleus point of RCell to be moved
    * @return int of RCell if valid move, else exception
    */
  private def validateMove(fx: RCell => RCell, nucleus: Point): Try[Int] = {
    val mover_idx = rCells.indexWhere(_.nucleus == nucleus)
    val mover = fx(rCells(mover_idx).copy())
    Try(
      if(mover_idx != -1)
        if(isValidCellState(mover))
          mover_idx
        else
          -1 //throw new BadMoveException("Invalid move from this position: " + rCells(mover_idx) + " " + dimensions)
      else
        throw new BadMoveException("You didn't select a cell to move")
    )
  }


  /**
    * determine if move is valid
    * @param cell cell to be evaluated
    * @return true if cell on board, sle false
    */
  private def isValidCellState(cell: RCell): Boolean = {
    onBoard(cell) && !isCollision(cell)
  }


  /**
    * determine if move is valid
    * @param cell cell to be evaluated
    * @return true if cell on board, sle false
    */
  private def onBoard(cell: Cell): Boolean = {
    val inRange = (i: Int) => i >= 0 && i <= dimensions
    inRange(cell.x1) && inRange(cell.x2) &&
      inRange(cell.y1) && inRange(cell.y2)
  }


  /**
    * determine if RCells are in same point
    * @param cell cell to be evaluated
    * @return true if cell is colliding, else false
    */
  private def isCollision(cell: RCell): Boolean = {
    rCells.exists(_.nucleus == cell.nucleus)
  }

  def boardStatus: Int = {
    val p1 = rCells.exists(_.marker == 1)
    val p2 = rCells.exists(_.marker == 2)
    if (p1 && p2) -1         // game in progress
    else if ( !p2 ) 1        // player 1 win
    else 2                   // opponent
  }

//  def boardStatus: Int = {
//    val p1 = rCells.exists(c => c.marker == 1 && c.x2 > 17)
//    val p2 = rCells.exists(c => c.marker == 2 && c.x1 < 3)
//    if (!p1 && !p2) -1         // game in progress
//    else if ( p1 ) 1           // player 1 win
//    else 2                     // opponent
//  }

  def playout(board: Board): Int = {

    val r = board.rCells.clone()
    val v = board.vCells.clone()
    val e = board.edges.map(_.clone)
    RandomGame.randomPlay(Board(r,v,e))
    //println("Chceck board edges not changed..")
    //e.foreach(e => {e.foreach(ee => System.out.print(ee)); println()})

  }

  def getAllPossibleNextBoard(player: Int): Seq[Board] = {
    val playerCells = rCells.filter(_.marker==player)
    val moves = new ListBuffer[Board]

    for(c <- playerCells) {
      moves += down(c.nucleus).get
      moves += left(c.nucleus).get
      moves += up(c.nucleus).get
      moves += right(c.nucleus).get
    }
    moves
  }

  /**
    * make 2DArray from board
    * @return 2DAarray from board
    */
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


  /**
    * print board using PrintStream
    * @param out output for printing board
    * @return Unit
    */
  def print(out: PrintStream = System.out): Unit = {
    for(i <- this.to2DArray.indices){
      for(j <- this.to2DArray.indices){
        if (to2DArray(i)(j) == 1) out.printf(s"${MAGENTA_B} *${RESET}")
        else if (to2DArray(i)(j) == 3) out.printf("%02d".format(rCells.indexWhere(_.nucleus == Point(j,i))).toString)
        else if (to2DArray(i)(j) == 2) out.printf(s"${BLUE_B} *${RESET}")
        else if (to2DArray(i)(j) == 6) out.printf(s"${RED_B}%02d".format(rCells.indexWhere(_.nucleus == Point(j,i))).toString)
        else if (to2DArray(i)(j) == 0) out.printf(s"${BLACK_B}  ${RESET}")
        else out.printf("")
      }
      out.println()
    }
  }


  /**
    * Turn board into csv format
    * @return board as csv format
    */
  def toCSV: String = {
    this.to2DArray.flatten.mkString(",")
  }
}


object Board { }
