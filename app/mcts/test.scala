package mcts

import game.{Point, RCell, VCell}
import model.Board

import scala.collection.mutable.ListBuffer
import scala.collection.parallel.ParSeq

/**
  * Created by kevin on 15/03/19.
  */
object test extends App{


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

  val size = 20

  val numPerTeam = size / 4

  val rCells = loadCells(0, numPerTeam, 1) ++ loadCells(size - 3, numPerTeam, 2)

  val Adj = Vector.fill[Vector[Int]](rCells.length)(Vector.fill[Int](rCells.length)(0))

  var board = Board(rCells, Vector(), Adj)

  val gameOver = false

  var player = 1

  val numCores = 7


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


//  case class Cell(list: ListBuffer[Int])
//
//  def changeCell(cell: Cell): Unit = {
//    var i = 0
//    while(i < 100000){
//      cell.list += i
//      i += 1
//    }
//    println(cell.list.length)
//  }
//
//    for(i <- (0 until Runtime.getRuntime.availableProcessors()).par) yield  changeCell(Cell(ListBuffer()))


  //result.foreach(r => println(r.x)


  // Prediction
  // 8 values of 10000 because different Cells are passed to par seq


//  val b1 = Board(Vector(RCell(1,1,4,4,1,1)), Vector(), Vector(Vector(1,1), Vector(2,2)))
//
//  val b2 = Board(Vector(RCell(1,1,4,4,1,1)), Vector(), Vector(Vector(1,1), Vector(2,2)))
//
//  val b3 = Board(Vector(RCell(1,1,4,4,1,1)), Vector(), Vector(Vector(1,1), Vector(2,3)))
//
////  println(b1 == b2)
////
////  println(b2 == b3)
//
  val t1 = Board(rCells,Vector(),Vector(Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 0, 1, 0, 0), Vector(0, 0, 0, 0, 0, 0, 1, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
//
//  val t2 = Board(rCells,Vector(),Vector(Vector(0, 1, 0, 0, 0, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 0, 1, 0, 0), Vector(0, 0, 0, 0, 0, 0, 1, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
//
//
//  println(t1.edges == t2.edges)
//  println(t1.rCells.toString(), t2.rCells.toString())
//  println(t1.vCells == t2.vCells)

  val s = State(t1,1)
  val n = Node(s,null)

  val t =
  for(i <- Range(0,5).par) yield i

  println(t)

}
