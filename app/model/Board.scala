package model

import java.io.PrintStream

import game.{Cell, Point, RCell, RandomGame}

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
  * @param edges  the graph of connections between rCells
  */
case class Board(rCells: Vector[RCell], vCells: Vector[Cell], edges: Vector[Vector[Int]]) {


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
  private def moveCell(fx: RCell => RCell, idx: Int): Vector[RCell] = synchronized {

    rCells.updated(idx, fx(rCells(idx)))

  }


  /**
    * Move Cell using fx on copy of [[Board.rCells]]
    * @param fx function to move cell up, down, left or right
    * @param idx index of cell to be moved in [[Board.rCells]]
    * @return new Board with moved/merged RCell
    */
  private def move(fx: RCell => RCell, idx: Int): Board = synchronized {

    // Move Real Cell
    val rCellsMoved = moveCell(fx, idx)

    // Function should return a new board, but not alter existing one
    // as a side effect
    val boardCopy = Board(rCellsMoved, vCells, edges)

    // Pattern match graph
    Graph(boardCopy).update(idx)  match {
      // Merges Needed
      case Some(Board(unconnected, connected, newEdges)) =>
        merge(newEdges, connected, unconnected, rCellsMoved, idx)
      // No merges needed
      case None => boardCopy
    }

  }


  /**
    * Merge all required VCells/RCells after a move
    * @param edges the graph of connections between RCells
    * @param connected the connected RCells in the graph which can form VCells
    * @param unconnected the unconnected RCells in the graph which cannot form VCells
    * @param rCellsMoved moved RCells of a board
    * @param idx index of cell to be moved in [[Board.rCells]]
    * @return new Board after moved/merged RCell
    */
  private def merge(edges: Vector[Vector[Int]],
                    connected: Vector[Cell],
                    unconnected: Vector[RCell],
                    rCellsMoved: Vector[RCell],
                    idx: Int): Board = synchronized {

    // Get Virtual Cells
    val vrMerged = rec(connected, unconnected, rCellsMoved, idx)

    // Capture Real Cells
    val capturedRCells = capture(rCellsMoved, vrMerged, idx)

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
  private def rec(connected: Vector[Cell],
                  unconnected: Vector[RCell],
                  rCellsCopy: Vector[RCell],
                  idx: Int): Vector[Cell] = synchronized{

    // Recursively merge V-V until no change
    val redVCells = recMergeVirtualCells(connected)

    // Recursively merge V-R until no change
    val vrMerged = recMergeVirtualAndReal(rCellsCopy(idx), redVCells, unconnected.asInstanceOf[Vector[Cell]])

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
  private def capture(rCells: Vector[RCell], vCells: Vector[Cell], idx: Int): Vector[RCell] = synchronized {

    var mutRCells = rCells
      for(i <- rCells.indices;
          j <- vCells.indices
          if vCells(j) contains rCells(i)) {
        mutRCells = mutRCells.updated(i,rCells(i).capture(vCells(j).marker).asInstanceOf[RCell])
      }

    mutRCells
  }


  /**
    * Recursively self merge virtual cells until stable
    * @param vc the VCells to checked for self-merges
    * @return list of self merged VCells
    */
  private def recMergeVirtualCells(vc: Vector[Cell]): Vector[Cell] = synchronized{

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
    * @param vc list of VCells
    * @param oc list of outer Cells (incl. RCells & VCells)
    * @return list of VCells
    */
  private def recMergeVirtualAndReal(mCell: RCell, vc: Vector[Cell], oc: Vector[Cell]): Vector[Cell] = synchronized{

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
  private def captureVCells(left: Cell, right: Cell, mCell: Cell): Cell = synchronized {
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
  private def validateMove(fx: RCell => RCell, nucleus: Point): Try[Int] = synchronized {
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
  private def isValidCellState(cell: RCell): Boolean = synchronized {
    onBoard(cell) && !isCollision(cell)
  }


  /**
    * determine if move is valid
    * @param cell cell to be evaluated
    * @return true if cell on board, sle false
    */
  private def onBoard(cell: Cell): Boolean = synchronized {
    val inRange = (i: Int) => i >= 0 && i <= dimensions
    inRange(cell.x1) && inRange(cell.x2) &&
      inRange(cell.y1) && inRange(cell.y2)
  }


  /**
    * determine if RCells are in same point
    * @param cell cell to be evaluated
    * @return true if cell is colliding, else false
    */
  private def isCollision(cell: RCell): Boolean = synchronized {
    rCells.exists(_.nucleus == cell.nucleus)
  }

  //  def boardStatus: Int = {
  //    val p1 = rCells.exists(_.marker == 1)
  //    val p2 = rCells.exists(_.marker == 2)
  //    if (p1 && p2) -1         // game in progress
  //    else if ( !p2 ) 1        // player 1 win
  //    else 2                   // opponent
  //  }

  def boardStatus: Int = synchronized {
    val p1 = rCells.exists(c => c.marker == 1 && (c.x2 > 19)) || rCells.forall(c => c.marker == Board.PLAYER1_WIN)
    val p2 = rCells.exists(c => c.marker == 2 && c.x1 < 1) || rCells.forall(c => c.marker == Board.PLAYER2_WIN)
    if (p1) Board.PLAYER1_WIN
    else if (p2) Board.PLAYER2_WIN
    else Board.IN_PROGRESS
  }

  def printEdges: Unit = synchronized {
    println("  " + (0 to edges.length-1).mkString(""))
    edges.indices.foreach(row => {System.out.print(row + " "); edges(row).foreach(i => System.out.print(i)); println()})
  }

  //def cloneBoard = copy(rCells = rCells.clone(), vCells = vCells.clone(), edges = edges.map(_.clone))

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
