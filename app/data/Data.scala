package data

import java.io.{BufferedWriter, FileWriter}

import game.RCell
import model.Board

import scala.util.{Failure, Random, Success, Try}
import scala.collection.mutable.ListBuffer

/**
  * Created by kevin on 07/03/19.
  */
object Data extends App {

  val random = new Random

  val size = 20

  val numPerTeam = size / 4


  def loadCells(x: Int, teamSize: Int, marker: Int): ListBuffer[RCell] = {

    for {
      y <- 0 until (teamSize * 4) by 4
    } yield RCell(x, y, x + 3, y + 3, marker)

  }.to[ListBuffer]

  val outputFile = new BufferedWriter(new FileWriter("/home/kevin/Documents/Projects/Functional_CellWars/app/data/data.csv")) //replace the path with the desired path and filename with the desired filename

  val csvFields = (0 until (size * size)).mkString(",") + "," + (0 until (size * size)).mkString(",")

  outputFile.write(csvFields)
  outputFile.write("\n")

  var i = 0
  while(i < 1000){

    val rCells = loadCells(7, numPerTeam, 1) ++ loadCells(size - 9, numPerTeam, 2)

    val Adj = Array.fill[Array[Int]](rCells.length)(Array.fill[Int](rCells.length)(0))

    var labelBoard = Board(rCells, ListBuffer(), Adj)

    var trainingBoard = labelBoard

    var b: Try[Board] = Success(labelBoard)

    labelBoard.print

    while(labelBoard.rCells.exists(_.marker == 1) && labelBoard.rCells.exists(_.marker == 2 && i < 1000)) {

      val selectedCell =
        if (i % 2 == 0) {
          val start = 0
          val end = rCells.size / 2
          val rnd = new scala.util.Random
          start + rnd.nextInt(end - start)
        } else {
          val start = rCells.size / 2
          val end = rCells.size
          val rnd = new scala.util.Random
          start + rnd.nextInt(end - start)
        }

      val selectedMove = getRandomElement(Seq(2, 4, 6, 8), random)

      b =
        selectedMove match {
          case 2 => labelBoard.down(labelBoard.rCells(selectedCell).nucleus)
          case 4 => labelBoard.left(labelBoard.rCells(selectedCell).nucleus)
          case 6 => labelBoard.right(labelBoard.rCells(selectedCell).nucleus)
          case 8 => labelBoard.up(labelBoard.rCells(selectedCell).nucleus)
        }

      b match {
        case Success(b) => labelBoard = b
        case Failure(e) => None
      }


      trainingBoard = Board(labelBoard.rCells, ListBuffer(), labelBoard.edges)
      outputFile.write(trainingBoard.toCSV + "," + labelBoard.toCSV)
      outputFile.write("\n")

      println("Move: " + i)
      //trainingBoard.print
      //println()
      labelBoard.print
      println()



      i += 1
    }
  }

  outputFile.close()

  def getRandomElement(list: Seq[Int], random: Random): Int =
    list(random.nextInt(list.length))

}
