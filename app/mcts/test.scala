package mcts

import game.{Point, RCell, VCell}
import model.Board
import org.apache.spark
import org.apache.spark.sql.SparkSession
import org.apache.spark.{SparkConf, SparkContext}
import org.graphframes.GraphFrame

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



  val spark = SparkSession.builder()
    .master("local[8]")
    .appName("CellWars")
    .getOrCreate()
  SparkSession.builder()


  // Vertex DataFrame
  val v = spark.sqlContext.createDataFrame(List(
    ("a", "Alice", 34),
    ("b", "Bob", 36),
    ("c", "Charlie", 30),
    ("d", "David", 29),
    ("e", "Esther", 32),
    ("f", "Fanny", 36),
    ("g", "Gabby", 60)
  )).toDF("id", "name", "age")


  // Edge DataFrame
  val e = spark.sqlContext.createDataFrame(List(
    ("a", "b", "friend"),
    ("b", "c", "follow"),
    ("c", "b", "follow"),
    ("f", "c", "follow"),
    ("e", "f", "follow"),
    ("e", "d", "friend"),
    ("d", "a", "friend"),
    ("a", "e", "friend")
  )).toDF("src", "dst", "relationship")


  val g = GraphFrame(v, e)

  g.vertices.show()

  val result = g.connectedComponents.run() // doesn't work on Spark 1.4

  result.show()




}
