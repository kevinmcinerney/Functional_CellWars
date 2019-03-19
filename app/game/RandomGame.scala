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
  def loadCells(x: Int, teamSize: Int, marker: Int): Vector[RCell] = {

    for {
      y <- 0 until (teamSize * 4) by 4
    } yield RCell(x, y, x + 3, y + 3, marker)

  }.toVector


  /**
    * PLay out random games
    */
  def randomPlay(board: Board): Int = synchronized {

    var player = 1

    var curBoard = board

    while(curBoard.boardStatus == Board.IN_PROGRESS) {

      val start = 0
      val teamOne = curBoard.rCells.filter(_.marker == player)
      val end = teamOne.length
      val rnd = new scala.util.Random
      val i = start + rnd.nextInt( (end - start) )
      val selectedCell = curBoard.rCells.indexOf(teamOne(i))

      val selectedMove = getRandomElement(Seq(2, 4, 6, 8))

      val movedBoard =
        selectedMove match {
          case 2 => curBoard.down(curBoard.copy().rCells(selectedCell).nucleus)
          case 4 => curBoard.left(curBoard.copy().rCells(selectedCell).nucleus)
          case 6 => curBoard.right(curBoard.copy().rCells(selectedCell).nucleus)
          case 8 => curBoard.up(curBoard.copy().rCells(selectedCell).nucleus)
        }

      curBoard =
        movedBoard match {
        case Success(brd) => brd
        case Failure(e) => curBoard
      }
      if (player == 1) player = 2 else player = 1
//      curBoard.print()
//      println()
    }
    curBoard.boardStatus
  }

  /**
    * Assign new team to Cell
    * @param list list from which to select a random item
    * @return random int
    */
  def getRandomElement(list: Seq[Int]): Int = synchronized {
    list(new Random().nextInt(list.length))
  }


  val teamOne = loadCells(7, 5, 1)

  //teamOne.update(0, RCell(34,2,37,5,1))

  val teamTwo = loadCells(12, 5, 2)

  val rCells = teamOne ++ teamTwo

  val emptyAdj = Vector.fill[Vector[Int]](rCells.length)(Vector.fill[Int](rCells.length)(0))

  val board = Board(rCells, Vector(), emptyAdj)

  randomPlay(board: Board)


}
