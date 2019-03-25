package game
import model.Board

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.{Failure, Random, Success, Try}
import java.net.ServerSocket
import java.io.PrintStream
import java.io.BufferedReader
import java.io.InputStreamReader
import java.net.Socket

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import java.util.concurrent.ConcurrentHashMap

import mcts.{MonteCarloTreeSearch, Node}

import scala.collection.JavaConverters._
import scala.collection.parallel.{ForkJoinTaskSupport, ParSeq}
import scala.collection.parallel.mutable.ParArray

/**
  * The Game Loop accessed via sockets.
  * Code used from @note https://github.com/MarkCLewis/OOAbstractDataStructScalaVideos/blob/master/src/networking/ChatServer.scala
  */
object SocketGame extends App {

  val size = 12

  val numPerTeam = size / 4

  val rCells = loadCells(0, numPerTeam, 1) ++ loadCells(size - 3, numPerTeam, 2)

  var board = Board(rCells, Vector())

  val gameOver = false

  var player = 1

  val numCores = 7

  /**
    * The Player.
    * @param name the players name
    * @param socket the socket use by the player
    * @param in the input used to receive messages
    * @param out  the output used to write messages
    */
  case class Player(name: String, socket: Socket, in: BufferedReader, out: PrintStream)

  val users = new ConcurrentHashMap[String, Player](2).asScala

  Future { checkConnections() }
  while(true) {
    for ((name, user) <- users) {
      startGame(user)
    }
    Thread.sleep(100)
  }


  /**
    * Repeatedly, asynchronously check socket for new users
    */
  def checkConnections(): Unit = {
    val ss = new ServerSocket(4000)
    while (true) {
      val sock = ss.accept()
      val in = new BufferedReader(new InputStreamReader(sock.getInputStream))
      val out = new PrintStream(sock.getOutputStream)
      Future {
        out.println("What is your name?")
        val name = in.readLine()
        val user = Player(name, sock, in ,out)
        if (users.size < 2)
        users += name -> user
        else out.println("Game is full")
      }
    }
  }


  /**
    * nonblockingRead
    */
  def nonblockingRead(in: BufferedReader): Option[String] = {
    if(in.ready()) Some(in.readLine()) else None
  }


  /**
    * start game between users
    */
  def startGame(user: Player): Unit = {
    val loop = (0 until numCores).par
    loop.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(8))
    nonblockingRead(user.in).foreach { input =>
      if(input == ":quit") {
        user.socket.close()
        users -= user.name
      } else {
        while(board.boardStatus == Board.IN_PROGRESS){
          for((n, u) <- users) {

            board.print(u.out)
            u.out.println()

            val start = 0
            val teamOne = board.rCells.filter(_.marker == player)
            val end = teamOne.length
            val rnd = new scala.util.Random
            val i = start + rnd.nextInt( end - start )
            val selectedCell = board.rCells.indexOf(teamOne(i))

            val selectedMove = getRandomElement(Seq(2, 4, 6, 8))

            val movedBoard =
              selectedMove match {
                case 2 => board.down(board.copy().rCells(selectedCell).nucleus)
                case 4 => board.left(board.copy().rCells(selectedCell).nucleus)
                case 6 => board.right(board.copy().rCells(selectedCell).nucleus)
                case 8 => board.up(board.copy().rCells(selectedCell).nucleus)
              }

            board =
              movedBoard match {
                case Success(brd) => brd
                case Failure(e) => board
              }


            board.print(u.out)
            u.out.println()

            player = if(player == 1) 2 else 1

            //========================================================
            val t1 = System.nanoTime()

            val result: ParSeq[Seq[Node]] =
              for(i <- loop) yield  MonteCarloTreeSearch().findNextMove(board, player, 180000)

            val idx = MonteCarloTreeSearch().bestMove(board, player, result)

            board = result.head(idx).state.board


//            for(position <- 0 until result.head.length){
//              val v1 = result(0)(position).state.board.vCells
//              val v2 = result(1)(position).state.board.vCells
//              val v3 = result(2)(position).state.board.vCells
//              val v4 = result(3)(position).state.board.vCells
//              val v5 = result(4)(position).state.board.vCells
//              val v6 = result(5)(position).state.board.vCells
//              val v7 = result(6)(position).state.board.vCells
//              val cores = List(v1, v2, v3,v4 ,v5, v6, v7)
//
//              for(
//                i <- cores.indices;
//                j <- cores.indices
//              if j < i && cores(i) != cores(j)
//              ){
//                println("cores: " + i + " and " + j + " for position: " + position)
//                println(cores(i))
//                println(cores(j))
//                println("==========================")
//                println("Boards: " + (result(i)(position).state.board == result(j)(position).state.board))
//                println("vCells: " + (result(i)(position).state.board.vCells == result(j)(position).state.board.vCells))
//                println("rCells: " + (result(i)(position).state.board.rCells == result(j)(position).state.board.rCells))
//
//                println("==========================")
////                result(i)(position).state.board.printEdges
////                println()
////                result(j)(position).state.board.printEdges
//                println("++========================")
//              }
//            }
//
//
//            val bools3 = new ListBuffer[Boolean]()
//            for(position <- 0 until result.head.length){
//              val v1 = result(0)(position).state.board.rCells == result(1)(position).state.board.rCells
//              val v2 = result(1)(position).state.board.rCells == result(2)(position).state.board.rCells
//              val v3 = result(2)(position).state.board.rCells == result(3)(position).state.board.rCells
//              val v4 = result(3)(position).state.board.rCells == result(4)(position).state.board.rCells
//              val v5 = result(5)(position).state.board.rCells == result(6)(position).state.board.rCells
//              bools3 += (v1 & v2 & v3 & v4 & v5)
//            }
//            println("rCells: " + bools3.forall(_==true))


            val t2 = System.nanoTime()
            println("Took: " + (t2 -t1) +  " seconds")


//
//              val selectedCell = getSelection(u.out, u.in, u.name, player)
//
//              val movedBoard = getMove(u.out, u.in, board, selectedCell)
//
//              board =
//                movedBoard match {
//                  case Success(brd) => brd
//                  case Failure(e) => board
//                }
              //===========================================

          }
          player = if(player == 1) 2 else 1
        }
        for((n, u) <- users) {
          if(board.boardStatus == 2) {
            u.out.print("COMPUTER WINS!")
            board.print(u.out)
          }
          else if(board.boardStatus == 1) {
            u.out.print("HUMAN WINS")
            board.print(u.out)
          }
          else{
            u.out.print("ERROR: WINNER UNKNOWN")
            board.print(u.out)
          }
        }
      }
    }
  }

  /**
    * Assign new team to Cell
    * @param list list from which to select a random item
    * @return random int
    */
  def getRandomElement(list: Seq[Int]): Int =
  list(new Random().nextInt(list.length))

  /**
    * Ask player for cell selection
    */
  def getSelection(out: PrintStream,in: BufferedReader, name: String, p_player: Int): Int = {

    out.println("Which cell are you moving," + name + "?")
    val selectedCell = in.readLine().toInt

    if(selectedCell < 0 || selectedCell >= rCells.length) {
      out.println("Invalid Pick. Try again")
      getSelection(out, in, name, p_player)
    }
    else if (p_player != rCells(selectedCell).marker){
      println(p_player, rCells(selectedCell).marker)
      out.println("Invalid Pick. Try again")
      getSelection(out, in, name, p_player)
    }
    else {
      selectedCell
    }
  }

  /**
    * Ask player for direction to move selected cell
    */
  def getMove(out: PrintStream, in: BufferedReader, board: Board, selected: Int): Try[Board] = {

    out.println("Which direction are you moving? (4) Left, (6) right, (2) down, (8) up")
    val move = in.readLine()
    val newBoard = move
    match {
      case "4" => board.left(board.rCells(selected).nucleus)
      case "6" => board.right(board.rCells(selected).nucleus)
      case "2" => board.down(board.rCells(selected).nucleus)
      case "8" => board.up(board.rCells(selected).nucleus)
      case _ =>   getMove(out, in, board, selected)
    }
  newBoard
  }


  /**
    * Assign new team to Cell
    * @param x_axis the x-axis on which to build team of Cells
    * @param teamSize number of Cells in a team
    * @param marker the team (1,2) to assign Cell
    * @return captured Cell
    */
  def loadCells(x: Int, teamSize: Int, marker: Int): Vector[RCell] = {

    val start = if(marker == 1) 0 else teamSize
    for {
      y <- 0 until (teamSize * 4) by 4
    } yield RCell(x, y, x + 3, y + 3, marker, (start + y/4))

  }.toVector



}
