package model

import java.io.PrintStream

import game.{Cell, Point, RCell}

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}
import Console.{BLACK_B, BLUE_B, MAGENTA_B, RED_B, RESET}


/**
  * Exception for invalid moves
  * @param msg message
  */
class BadMoveException(msg: String) extends Exception(msg)


/**
  * The Board representation.
  * @param rCells the RCells on the board
  * @param vCells the VCells on the board
  */
case class Board(rCells: Vector[RCell], vCells: Vector[Cell]) {


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
    * Add edges to cells after a move
    * @param mover the moved cell
    * @param rCellsMoved all cells, incl moved one
    * @param mover_idx index of moved cell in rCellsMoved
    * @return list of all cells with some edges added due to moved cell
    */
  def getMovedRCellsWithEdges(mover: RCell, rCellsMoved: Vector[RCell], mover_idx: Int): Vector[RCell] = {

    val vector =
    rCellsMoved.foldLeft(Vector[RCell](mover)) {
      (acc, next) =>
        if (mover.id == next.id) {acc ++ Vector(next)}
        else if (!(mover contains next) && !(acc contains next)) { acc ++ Vector(next) }
        else {
          val updated = acc.updated(0, acc(0).copy(edges = acc(0).edges :+ next.id))
          updated ++ Vector(next.addEdge(mover))
        }
      }
    vector.updated(mover_idx + 1, vector(0)).tail
    }


  /**
    * Remove edges from cells after a move
    * @param rCellsPreMove list of cells before move
    * @param rCellsMoved list of cells after move
    * @param mover_idx index of moved cell in rCellsMoved
    * @return list of all cells some with edges removed due to moved cell
    */
  def getRCellsWithoutEdges(rCellsPreMove: Vector[RCell], rCellsMoved: Vector[RCell], mover_idx: Int): Vector[RCell] = {

    val updated =
      (for( i <- rCellsPreMove.indices;
         temp =
         if ((rCellsPreMove(i) contains rCellsPreMove(mover_idx)) && (i != mover_idx)) {
           rCellsMoved(i).delEdge(rCellsPreMove(mover_idx))
         }else{
           rCellsMoved(i)
         }
    ) yield temp).toVector

    if(updated.isEmpty) rCellsMoved else updated
  }


  /**
    * Check is rCell inside any vCell
    * @param rCell rCell to evalute
    * @return true if rCell is inside of one of VCells
    */
  private def isInsideVCell(rCell: RCell, vCells: Vector[Cell]): Boolean =  {
    vCells.exists(v => v contains rCell)
  }


  /**
    * Move Cell using fx
    * @param fx function to move cell up, down, left or right
    * @param mover_idx index of cell to be moved in [[Board.rCells]]
    * @return new Board with moved/merged RCell
    */
  private def move(fx: RCell => RCell, mover_idx: Int): Board =  {

    // Move Real Cell
    val rCellsMoved = rCells.updated(mover_idx, fx(rCells(mover_idx)))

    // Remove edges
    val rCellsMovedWithoutEdges = getRCellsWithoutEdges(rCells, rCellsMoved, mover_idx)

    // Add edges
    val rCellsMovedWithEdges = getMovedRCellsWithEdges(rCellsMovedWithoutEdges(mover_idx), rCellsMovedWithoutEdges, mover_idx)

    // Find Connected Components
    Graph().update(rCellsMovedWithEdges,
                   rCellsMovedWithEdges == rCellsMoved,
                   isInsideVCell(rCellsMoved(mover_idx), vCells),
                   mover_idx)  match {
      // Merges Needed
      case Some(Board(free, connected)) =>
        merge(connected, free, rCellsMovedWithEdges, mover_idx)
      // No merges needed
      case None => Board(rCellsMovedWithEdges, vCells)
    }

  }


  /**
    * Merge all required vCells/vCells after a move
    * @param connected the connected rCells in the graph
    * @param free the unconnected rCells in the graph
    * @param allUpdatedCells moved rCells with all updated edges
    * @param mover_idx index of moved cell
    * @return new Board after moved/merged RCell
    */
  private def merge(connected: Vector[Cell],
                    free: Vector[RCell],
                    allUpdatedCells:  Vector[RCell],
                    mover_idx: Int): Board =  {

    // Get Virtual Cells
    val vrMerged = rec(connected, free, allUpdatedCells, mover_idx)

    // Capture Real Cells
    val capturedRCells = capture(allUpdatedCells, vrMerged, mover_idx)

    Board(capturedRCells, vrMerged)
  }


  /**
    * Recursively 1) Merge virtual-virtual cells and 2) virtual-real cells until stable
    * @param connected the connected rCells in the graph
    * @param free the unconnected rCells in the graph
    * @param rCellsMovedWithEdges all cells moved with edges
    * @param mover_idx index of cell to be moved in [[Board.rCells]]
    * @return List of new Virtual Cells
    */
  private def rec(connected: Vector[Cell],
                  free: Vector[RCell],
                  rCellsMovedWithEdges:  Vector[RCell],
                  mover_idx: Int): Vector[Cell] = {

    // Recursively merge V-V until no change
    val redVCells = recMergeVirtualCells(connected)

    // Recursively merge V-R until no change
    val vrMerged = recMergeVirtualAndReal(rCellsMovedWithEdges(mover_idx), redVCells, free.asInstanceOf[Vector[Cell]])

    // Recursively call self until no change
    if(connected == vrMerged) connected else rec(vrMerged, free, rCellsMovedWithEdges, mover_idx)
  }


  /**
    * Change team of rCells inside of expanded vCells
    * @param rCells the rCells on the board
    * @param vCells the vCells on the board
    * @param mover_idx index of cell to be moved
    * @return list of cells with captured cells assigned to other team
    */
  private def capture(rCells: Vector[RCell], vCells: Vector[Cell], mover_idx: Int): Vector[RCell] =  {

    val expandingVCell = vCells.find(_.contains(rCells(mover_idx)))
    if (expandingVCell.nonEmpty){
      (
        for(i <- rCells.indices;
            cell = if (expandingVCell.get contains rCells(i)) rCells(i).copy(marker = expandingVCell.get.marker)
                   else rCells(i)
        ) yield cell
        ).toVector
    }else{
      rCells
    }
  }


  /**
    * Recursively self merge virtual cells until stable
    * @param vc the VCells to be checked for self-merges
    * @return list of self merged VCells
    */
  private def recMergeVirtualCells(vc: Vector[Cell]): Vector[Cell] = {

    val vcc = new ListBuffer[Cell]()
    vc.foreach((v: Cell) => vcc += v)
    val pairs = vc.combinations(2).to[ListBuffer]

    val merges = pairs.filter(p => p.head contains p.tail.head)

    if(merges.nonEmpty){
      val newVCells = merges.map(pair => pair.head merge pair.tail.head)
      merges.foreach(m => {vcc -= m.head; vcc -= m.tail.head} )
      recMergeVirtualCells((newVCells ++ vcc).toVector)
    }
    else { vc }
  }


  /**
    * Recursively merge VCells with RCells until stable
    * @param mCell the moved RCell
    * @param vc list of vCells
    * @param oc list of outer Cells (incl. vCells & vCells)
    * @return list of vCells
    */
  private def recMergeVirtualAndReal(mCell: RCell, vc: Vector[Cell], oc: Vector[Cell]): Vector[Cell] = {

    val occ = new ListBuffer[Cell]()
    oc.foreach((o: Cell) => occ += o)
    val pairs = vc.flatMap(v => oc.map(o => (v, o)))
    val newVCells: Vector[Cell] =
      for((left, right) <- pairs if left contains right) yield {
        occ -= (left,right)
        captureVCells(left,right,mCell)
      }
    if(newVCells.nonEmpty) recMergeVirtualAndReal(mCell, recMergeVirtualCells(newVCells ++ vc), occ.toVector)
    else vc
  }


  /**
    * Recursively merge VCells with RCells until stable
    * @param left cell to be merged
    * @param right cell to be merged
    * @param mCell the moved RCell whose team will capture the result of merge
    * @return Captured Cell with correct team assigned
    */
  private def captureVCells(left: Cell, right: Cell, mCell: Cell): Cell =  {
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
  private def validateMove(fx: RCell => RCell, nucleus: Point): Try[Int] =  {
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
  private def isValidCellState(cell: RCell): Boolean =  {
    onBoard(cell) && !isCollision(cell)
  }


  /**
    * determine if move is valid
    * @param cell cell to be evaluated
    * @return true if cell on board, sle false
    */
  private def onBoard(cell: Cell): Boolean =  {
    val inRange = (i: Int) => i >= 0 && i <= dimensions
    inRange(cell.x1) && inRange(cell.x2) &&
      inRange(cell.y1) && inRange(cell.y2)
  }


  /**
    * determine if RCells are in same point
    * @param cell cell to be evaluated
    * @return true if cell is colliding, else false
    */
  private def isCollision(cell: RCell): Boolean =  {
    rCells.exists(_.nucleus == cell.nucleus)
  }


  def boardStatus: Int =  {
    val p1 = rCells.exists(c => c.marker == 1 && (c.x2 > this.dimensions-1)) || rCells.forall(c => c.marker == Board.PLAYER1_WIN)
    val p2 = rCells.exists(c => c.marker == 2 && c.x1 < 1) || rCells.forall(c => c.marker == Board.PLAYER2_WIN)
    if (p1) Board.PLAYER1_WIN
    else if (p2) Board.PLAYER2_WIN
    else Board.IN_PROGRESS
  }


  /**
    * make 2DArray from board
    * @return 2DAarray from board
    */
  def to2DArray: Vector[Array[Int]] = {

    val matrix = Array.fill[Int](dimensions,dimensions) { 0 }

    def addReal(cells: Array[RCell]): Vector[Array[Int]] ={
      for{ c <- cells
           p <- c.drawPoints
      }  matrix(p.y)(p.x) = c.marker //matrix.updated(p.y, matrix(p.y).updated(p.x, c.marker))
      matrix.toVector
    }

    def addVirtual(cells: Array[Cell]): Vector[Array[Int]] ={
      for{ c <- cells
           p <- c.drawPoints
      } matrix(p.y)(p.x) = c.marker //matrix.updated(p.y, matrix(p.y).updated(p.x, c.marker))
      matrix.toVector
    }

    def addNucleus(cells: Array[RCell]): Vector[Array[Int]] = {
      for{ c <- cells
      } matrix(c.nucleus.y)(c.nucleus.x) = c.marker * 3 //matrix.updated(c.nucleus.y, matrix(c.nucleus.y).updated(c.nucleus.x, c.marker * 3)) //matrix(c.nucleus.y)(c.nucleus.x) = c.marker * 3
      matrix.toVector

    }

    addReal(rCells.toArray)

    addVirtual(vCells.toArray)

    addNucleus(rCells.toArray)

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


object Board {
  val IN_PROGRESS = -1
  val PLAYER1_WIN = 1
  val PLAYER2_WIN = 2
  val WIN_SCORE = 10
}
