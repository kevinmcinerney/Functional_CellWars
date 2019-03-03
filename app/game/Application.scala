package game
import model.Board
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}
import java.net.ServerSocket
import java.io.PrintStream
import java.io.BufferedReader
import java.io.InputStreamReader
import java.net.Socket
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import java.util.concurrent.ConcurrentHashMap
import scala.collection.JavaConverters._

/**
  * Created by kevin on 26/01/19.
  */
object Application extends App{

  val size = 20

  val numPerTeam = size / 4


  def loadCells(x: Int, teamSize: Int, marker: Int): ListBuffer[RCell] = {

    for {
      y <- 0 until (teamSize * 4) by 4
    } yield RCell(x, y, x + 3, y + 3, marker)

  }.to[ListBuffer]

  val rCells = loadCells(0, numPerTeam, 1) ++ loadCells(size - 3, numPerTeam, 2)

  val Adj = Array.fill[Array[Int]](rCells.length)(Array.fill[Int](rCells.length)(0))

  var board = Board(rCells, ListBuffer(), Adj)

  val gameOver = false

  var player = 1

  case class User(name: String, sock: Socket, in: BufferedReader, out: PrintStream)
  val users = new ConcurrentHashMap[String, User](2).asScala

  Future { checkConnections() }
  while(true) {
    for ((name, user) <- users) {
      doChat(user)
    }
    Thread.sleep(100)
  }

  def checkConnections(): Unit = {
    val ss = new ServerSocket(4000)
    while (true) {
      val sock = ss.accept()
      val in = new BufferedReader(new InputStreamReader(sock.getInputStream))
      val out = new PrintStream(sock.getOutputStream)
      Future {
        out.println("What is your name?")
        val name = in.readLine()
        val user = User(name, sock, in ,out)
        if (users.size < 2)
        users += name -> user
        else out.println("Game is full")
      }
    }
  }

  def nonblockingRead(in: BufferedReader): Option[String] = {
    if(in.ready()) Some(in.readLine()) else None
  }

  def doChat(user: User): Unit = {
    nonblockingRead(user.in).foreach { input =>
      if(input == ":quit") {
        user.sock.close()
        users -= user.name
      } else {
        while(!gameOver){
          for((n, u) <- users) {

            board.print(u.out)

            val selected = getSelection(u.out, u.in, n, player)

            val move = getMove(u.out, u.in, board, selected)

            board =
              move match {
                case Success(b) => b
                case Failure(m) => board
              }

            for((n,u) <- users) {board.print(u.out); u.out.println()}
            player = if(player == 1) 2 else 1
          }
        }
      }
    }
  }

    def getSelection(out: PrintStream,in: BufferedReader, name: String, p_player: Int): Int = {

      out.println("Which cell are you moving," + name + "?")
      val test = in.readLine()
      println(test)
      val selectedCell = test.toInt

      if(selectedCell < 0 || selectedCell >= rCells.length) {
        out.println("Invalid Pick. Try again")
        getSelection(out, in, name, p_player)
      }
      else if ((selectedCell > 0 || selectedCell < rCells.length) && p_player != rCells(selectedCell).marker){
        out.println("Invalid Pick. Try again")
        getSelection(out, in, name, p_player)
      } else {
        selectedCell
      }
    }

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



}
