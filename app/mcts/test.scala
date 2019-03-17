//package mcts
//
//import game.{Point, RCell, VCell}
//import model.Board
//
//import scala.collection.mutable.ListBuffer
//
///**
//  * Created by kevin on 15/03/19.
//  */
//object test extends App{
//
//
//  /**
//    * Assign new team to Cell
//    * @param x_axis the x-axis on which to build team of Cells
//    * @param teamSize number of Cells in a team
//    * @param marker the team (1,2) to assign Cell
//    * @return captured Cell
//    */
//  def loadCells(x_axis: Int, teamSize: Int, marker: Int): ListBuffer[RCell] = {
//    val start = if(marker == 1) 0  else teamSize
//    for {
//      y <- 0 until (teamSize * 4) by 4
//    } yield RCell(x_axis, y, x_axis + 3, y + 3, marker, start + y /4)
//
//  }.to[ListBuffer]
//
//  val size = 20
//
//  val numPerTeam = size / 4
//
//  val rCells = loadCells(0, numPerTeam, 1) ++ loadCells(size - 3, numPerTeam, 2)
//
//  val Adj = Array.fill[Array[Int]](rCells.length)(Array.fill[Int](rCells.length)(0))
//
//  var board = Board(rCells, ListBuffer(), Adj)
//
//  val gameOver = false
//
//  var player = 1
//
//  val numCores = 7
//
//
//  board.print()
//  val b = board.down(Point(1,1)) // b has no edges
//  b.get.print()
//  val b2 = b.get.down(Point(1,2)) //
//  b2.get.print()
//  val b3 = b2.get.right(Point(1,3))
//  b3.get.print()
//
//  board.print()
//
//  b2.get.edges.foreach(row => {row.foreach(i => print(i)); println()})
//
//
//
//}
