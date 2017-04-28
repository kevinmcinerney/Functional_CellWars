import org.scalatestplus.play.PlaySpec
import game.Game._


/**
  * Created by kevin on 25/04/17.
  */
class GameSpec extends PlaySpec {

  val board = new Board(20)
  val cell1 = Cell(Coordinate(0,0),Coordinate(2, 2))
  val cell2 = Cell(Coordinate(17,17),Coordinate(19, 19))

  "board" should {

    "not allow values < than 0" in {
      println((-10 to -1).map(n => board.onBoard(n)))
      (-10 to -1).map(n => board.onBoard(n)).forall(_ == false) mustBe true
    }

    "not allow values >= than dimensions" in {
      (board.dimensions to 30).map(n => board.onBoard(n)).forall(_ == false) mustBe true
    }

    "allow values >= than 0 and < dimensions" in {
      (0 until board.dimensions).map(n => board.onBoard(n)).forall(_ == true) mustBe true
    }
  }

  "cell (0,0)-(2,2)" should {

    "not move up" in {
      board isValidMove(board.up, cell1) mustBe false
    }
    "not move left" in {

      board isValidMove(board.left, cell1) mustBe false

    }
    "can move down" in  {
      board isValidMove(board.down, cell1) mustBe true
  }
    "can move right" in {
      board isValidMove(board.right, cell1) mustBe true}
  }

  "cell (17,17)-(19,19)" should {

    "not move up" in {
      board isValidMove(board.up, cell2) mustBe true
    }
    "not move left" in {

      board isValidMove(board.left, cell2) mustBe true

    }
    "can move down" in  {
      board isValidMove(board.down, cell2) mustBe false
    }
    "can move right" in {
      board isValidMove(board.right, cell2) mustBe false
    }
  }

}
