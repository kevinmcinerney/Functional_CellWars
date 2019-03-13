package game

import mcts.MonteCarloTreeSearch
import model.Board

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Random, Success, Try}

/**
  * Playout full games using random moves
  */
object RandomGame extends App {

  /**
    * Assign new team to Cell
    * @param x_axis the x-axis on which to build team of Cells
    * @param teamSize number of Cells in a team
    * @param marker the team (1,2) to assign Cell
    * @return captured Cell
    */
  def loadCells(x_axis: Int, teamSize: Int, marker: Int): ListBuffer[RCell] = {
    for {
      y <- 0 until (teamSize * 4) by 4
    } yield RCell(x_axis, y, x_axis + 3, y + 3, marker)

  }.to[ListBuffer]


  /**
    * PLay out random games
    */
  def randomPlay(board: Board): Int = {

    var i = 0

    var curBoard = board

    while(curBoard.boardStatus == -1) {

      val selectedCell =
        if (i % 2 == 0) {
          val start = 0
          val teamOne = curBoard.rCells.filter(_.marker==1)
          val end = teamOne.length
          val rnd = new scala.util.Random
          val i = start + rnd.nextInt( (end - start) )
          curBoard.rCells.indexOf(teamOne(i))
        } else {
          val start = 0
          val teamTwo = curBoard.rCells.filter(_.marker==2)
          val end = teamTwo.length
          val rnd = new scala.util.Random
          val i = start + rnd.nextInt( (end - start) )
          curBoard.rCells.indexOf(teamTwo(i))
        }

      val selectedMove = getRandomElement(Seq(2, 4, 6, 8))

      val movedBoard =
        selectedMove match {
          case 2 => curBoard.down(curBoard.rCells(selectedCell).nucleus)
          case 4 => curBoard.left(curBoard.rCells(selectedCell).nucleus)
          case 6 => curBoard.right(curBoard.rCells(selectedCell).nucleus)
          case 8 => curBoard.up(curBoard.rCells(selectedCell).nucleus)
        }

      curBoard =
        movedBoard match {
        case Success(brd) => brd
        case Failure(e) => curBoard
      }
      i += 1
//      curBoard.print()
//      println()
    }
    //curBoard.print()
    curBoard.boardStatus

  }

  /**
    * Assign new team to Cell
    * @param list list from which to select a random item
    * @return random int
    */
  def getRandomElement(list: Seq[Int]): Int =
    list(new Random().nextInt(list.length))


  /**
    * PLay out random games
    * @param moves number of moves to playout through multiple games
    */
  def randomPlay(moves: Int): Unit = {

    //    val outputFile = new BufferedWriter(new FileWriter("/home/kevin/Documents/Projects/Functional_CellWars/app/data/data.csv")) //replace the path with the desired path and filename with the desired filename
    //
    //    val csvFields = (0 until (size * size)).mkString(",") + "," + (0 until (size * size)).mkString(",")
    //
    //    outputFile.write(csvFields)
    //    outputFile.write("\n")

    var i = 0

    while(i < moves){

      val rCells = loadCells(7, 5, 1) ++ loadCells(11, 5, 2)

      val Adj = Array.fill[Array[Int]](rCells.length)(Array.fill[Int](rCells.length)(0))

      var curBoard = Board(rCells, ListBuffer(), Adj)

      //var trainingBoard = labelBoard

      var b: Try[Board] = Success(curBoard)

      //b.get.print()

      while(curBoard.rCells.exists(_.marker == 1) && curBoard.rCells.exists(_.marker == 2)) {

        val selectedCell =
          if (i % 2 == 0) {
            val start = 0
            val teamOne = curBoard.rCells.filter(_.marker==1)
            val end = teamOne.length
            val rnd = new scala.util.Random
            val i = start + rnd.nextInt( (end - start) )
            curBoard.rCells.indexOf(teamOne(i))
          } else {
            val start = 0
            val teamTwo = curBoard.rCells.filter(_.marker==2)
            val end = teamTwo.length
            val rnd = new scala.util.Random
            val i = start + rnd.nextInt( (end - start) )
            curBoard.rCells.indexOf(teamTwo(i))
          }

        val selectedMove = getRandomElement(Seq(2, 4, 6, 8))

        val movedBoard =
          selectedMove match {
            case 2 => curBoard.down(curBoard.rCells(selectedCell).nucleus)
            case 4 => curBoard.left(curBoard.rCells(selectedCell).nucleus)
            case 6 => curBoard.right(curBoard.rCells(selectedCell).nucleus)
            case 8 => curBoard.up(curBoard.rCells(selectedCell).nucleus)
          }

        curBoard =
          movedBoard match {
            case Success(brd) => brd
            case Failure(e) => curBoard
          }


        //        trainingBoard = Board(labelBoard.rCells, ListBuffer(), labelBoard.edges)
        //        outputFile.write(trainingBoard.toCSV + "," + labelBoard.toCSV)
        //        outputFile.write("\n")

        //println("Move: " + i)
        //trainingBoard.print
        //println()
//        b.get.print()
//        println()

        i += 1

      }
      curBoard.print()
    }

    //outputFile.close()
  }

  val teamOne = loadCells(7, 5, 1)

  //teamOne.update(0, RCell(34,2,37,5,1))

  val teamTwo = loadCells(12, 5, 2)

  val rCells = teamOne ++ teamTwo

  val emptyAdj = Array.fill[Array[Int]](rCells.length)(Array.fill[Int](rCells.length)(0))

  val board = Board(rCells, ListBuffer(), emptyAdj)

  randomPlay(board: Board)


}
