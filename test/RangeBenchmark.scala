import game.RCell
import mcts.{MonteCarloTreeSearch, Node}
import model.Board
import org.scalameter._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.collection.parallel.ParSeq
import scala.collection.parallel.mutable.ParArray

object RangeBenchmark extends  App{


  /**
    * Assign new team to Cell
    * @param x_axis the x-axis on which to build team of Cells
    * @param teamSize number of Cells in a team
    * @param marker the team (1,2) to assign Cell
    * @return captured Cell
    */
  def loadCells(x_axis: Int, teamSize: Int, marker: Int): Vector[RCell] = {
    val start = if(marker == 1) 0  else teamSize
    for {
      y <- 0 until (teamSize * 4) by 4
    } yield RCell(x_axis, y, x_axis + 3, y + 3, marker, start + y /4)

  }.toVector

  val teamOne = loadCells(0, 5, 1)

  val teamTwo = loadCells(17, 5, 2)

  val rCells = teamOne ++ teamTwo

  val emptyAdj = Vector.fill[Vector[Int]](rCells.length)(Vector.fill[Int](rCells.length)(0))

  val board = Board(rCells, Vector(), emptyAdj)

  val numCores = 7

  val time2 = measure {

    val result: ParSeq[Seq[Node]] =
      for(i <- (0 until numCores).par) yield  MonteCarloTreeSearch().findNextMove(board, 2, 1000)

    val idx = MonteCarloTreeSearch().bestMove(board, 2, result)

    result.head(idx).state.board.print()
  }
  println(s"Total time: $time2")




  val time = measure {
    val result: ParSeq[Seq[Node]] =
      for(i <- (0 until numCores).par) yield  MonteCarloTreeSearch().findNextMove(board, 2, 1000)

    val idx = MonteCarloTreeSearch().bestMove(board, 2, result)

    result.head(idx).state.board.print()
  }
  println(s"Total time: $time")


  // Before
  //Total time: 8375.565843
  //Total time: 8417.430402
  //Total time: 8464.456001
  //Total time: 8278.768044
  //Total time: 7506.877677


  // After contains
  //Total time: 631.300545
  //Total time: 593.93004
  //Total time: 562.482969
  //Total time: 561.864561

  //Before Again
  //Total time: 7319.85556
  //Total time: 8419.658983
  //Total time: 9009.91471
  //Total time: 7135.215225

  //After Again
  //Total time: 528.822745
  //Total time: 547.383891
  //Total time: 514.375074
  //Total time: 533.076591


}