import game.RCell
import model.Board
import org.scalameter._

import scala.collection.mutable.ListBuffer

object RangeBenchmark extends  App{


  def loadCells(x_axis: Int, teamSize: Int, marker: Int): ListBuffer[RCell] = {
    for {
      y <- 0 until (teamSize * 4) by 4
    } yield RCell(x_axis, y, x_axis + 3, y + 3, marker)

  }.to[ListBuffer]

  val teamOne = loadCells(7, 5, 1)

  //teamOne.update(0, RCell(34,2,37,5,1))

  val teamTwo = loadCells(12, 5, 2)

  val rCells = teamOne ++ teamTwo

  val emptyAdj = Array.fill[Array[Int]](rCells.length)(Array.fill[Int](rCells.length)(0))

  val board = Board(rCells, ListBuffer(), emptyAdj)

  val time = measure {
    import game.RandomGame
    RandomGame.randomPlay(board)
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