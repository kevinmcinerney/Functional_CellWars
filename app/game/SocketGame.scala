package game
import model.Board

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Random, Success, Try}
import java.net.ServerSocket
import java.io.PrintStream
import java.io.BufferedReader
import java.io.InputStreamReader
import java.net.Socket

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import java.util.concurrent.ConcurrentHashMap

import mcts.MonteCarloTreeSearch

import scala.collection.JavaConverters._

/**
  * The Game Loop accessed via sockets.
  * Code used from @note https://github.com/MarkCLewis/OOAbstractDataStructScalaVideos/blob/master/src/networking/ChatServer.scala
  */
object SocketGame extends App {

  val size = 20

  val numPerTeam = size / 4

  val rCells = loadCells(0, numPerTeam, 1) ++ loadCells(size - 3, numPerTeam, 2)

  val Adj = Array.fill[Array[Int]](rCells.length)(Array.fill[Int](rCells.length)(0))

  var board = Board(rCells, ListBuffer(), Adj)

  val gameOver = false

  var player = 1

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
    val ss = new ServerSocket(1000)
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
    nonblockingRead(user.in).foreach { input =>
      if(input == ":quit") {
        user.socket.close()
        users -= user.name
      } else {
        while(board.boardStatus == -1){
          for((n, u) <- users) {

            board.print(u.out)
            u.out.println()

            println()
            if(player == 1){

              print("Player 1: ")

              val start = 0
              val teamOne = board.rCells.filter(_.marker==1)
              val end = teamOne.length
              val rnd = new scala.util.Random
              val i = start + rnd.nextInt( (end - start) )
              val selectedCell = board.rCells.indexOf(teamOne(i))

              val selectedMove = getRandomElement(Seq(2, 4, 6, 8))

              val movedBoard =
                selectedMove match {
                  case 2 => board.down(board.rCells(selectedCell).nucleus)
                  case 4 => board.left(board.rCells(selectedCell).nucleus)
                  case 6 => board.right(board.rCells(selectedCell).nucleus)
                  case 8 => board.up(board.rCells(selectedCell).nucleus)
                }

              board =
                movedBoard match {
                  case Success(brd) => brd
                  case Failure(e) => board
                }

              //board = MonteCarloTreeSearch.findNextMove(board, 1000, player)

//              val selected = getSelection(u.out, u.in, n, player)
//              val move = getMove(u.out, u.in, board, selected)
//
//              board =
//                move match {
//                  case Success(b) => b
//                  case Failure(m) => board
//                }

            }else{
              print("Player 2: ")
              board = MonteCarloTreeSearch.findNextMove(board, 50000, player)
            }

            player = if(player == 1) 2 else 1
          }
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
  def loadCells(x_axis: Int, teamSize: Int, marker: Int): ListBuffer[RCell] = {
    for {
      y <- 0 until (teamSize * 4) by 4
    } yield RCell(x_axis, y, x_axis + 3, y + 3, marker)

  }.to[ListBuffer]



}
