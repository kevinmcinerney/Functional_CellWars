//import org.scalatestplus.play.PlaySpec
//import game._
//import model.{Board, Graph}
//import org.scalatest.{FlatSpec, PrivateMethodTester}
//
//import scala.collection.mutable.ListBuffer
//
//
//
///**
//  * Created by kevin on 25/04/17.
//  */
//class GameSpec extends PlaySpec with PrivateMethodTester {
//
//  def time[R](block: => R): String = {
//    val t0 = System.nanoTime()
//    val t1 = System.nanoTime()
//    "Elapsed time: " + (t1 - t0) + "ms"
//  }
//
//  val cell1 = RCell(0, 0, 3, 3, 1,0)
//  val cell2 = RCell(3, 0, 6, 3, 1,1)
//  val cell3 = RCell(2, 3, 5, 6, 1,2)
//  val cell4 = RCell(5, 5, 8, 8, 1,3)
//  val cell5 = RCell(2, 7, 5, 10, 1,4)
//  val cell6 = RCell(6, 0, 9, 3, 1,5)
//  val cell7 = RCell(9, 2, 12, 5, 1,6)
//
//  val cell8 = RCell(9, 9, 12, 12, 2,7)
//  val cell9 = RCell(17, 0, 20, 3, 2,8)
//  val cell10 = RCell(17, 6, 20, 9, 2,9)
//  val cell11 = RCell(17, 9, 20, 12, 2,10)
//  val cell12 = RCell(17, 12, 20, 15, 2,11)
//  val cell13 = RCell(12, 12, 15, 15, 2,12)
//  val cell14 = RCell(15, 15, 18, 18, 2,13)
//
//  val rCells_t1 = Vector(cell1, cell2, cell3, cell4, cell5, cell6, cell7, cell8, cell9, cell10, cell11, cell12, cell13, cell14)
//  val rCells_t2 = Vector(cell1, cell2, cell3.right, cell4, cell5, cell6, cell7, cell8, cell9, cell10, cell11, cell12, cell13, cell14)
//  val rCells_t3 = Vector(cell1, cell2, cell3.right, cell4, cell5.right, cell6, cell7, cell8, cell9, cell10, cell11, cell12, cell13, cell14)
//  val rCells_t4 = Vector(cell1, cell2, cell3.right, cell4, cell5.right, cell6.right, cell7, cell8.capture(1).asInstanceOf[RCell], cell9, cell10, cell11, cell12, cell13, cell14)
//
//  val board_t1 = Board(rCells_t1, Vector())
//  println("board_t1")
//  board_t1.print()
//  println()
//
////  Adj(2)(3) = 1
////  Adj(3)(2) = 1
////  Adj(3)(4) = 1
////  Adj(4)(3) = 1
////  val a1 = Graph(board_t1).updateEdge(2,3,1,Adj)
////  val a2 = Graph(board_t1).updateEdge(3,4,1,a1)
//
//  val rCells_t2 = Vector(cell1, cell2., cell3.right, cell4, cell5, cell6, cell7, cell8, cell9, cell10, cell11, cell12, cell13, cell14)
//
//  val board_t2 = Board(rCells_t2, Vector(VCell(2, 3, 8, 10, 1)))
//  println("board_t2: cell3 right")
//  board_t2.print()
//  println()
//
//  val board_t3 = Board(rCells_t3, Vector(VCell(3, 3, 8, 10, 1)))
//  println("board_t3: cell3 right, cell5 right")
//  board_t3.print()
//  println()
//
////  Adj(5)(6) = 1
////  Adj(6)(5) = 1
////  val a3 = Graph(board_t1).updateEdge(5,6,1,a2)
//
//  val board_t4 = Board(rCells_t4, Vector(VCell(3, 0, 12, 12, 1))).addEdge
//
//  println("board_t4: Before Right-Right-Left-Left: cell3 right, cell5 right, cell6 right")
//  //println(board_t4.rCells)
//  board_t4.print()
//  println()
//
//  "right-right-left-left" should {
//    "for " + cell1 + " should be same " in {
//      //board_t4.right(Point(1, 1)).get.right(Point(2, 1)).get.left(Point(3, 1)).get.left(Point(2, 1)).get.edges mustEqual board_t4.edges
//      board_t4.right(Point(1, 1)).get.right(Point(2, 1)).get.left(Point(3, 1)).get.left(Point(2, 1)).get.rCells mustEqual board_t4.rCells
//      board_t4.right(Point(1, 1)).get.right(Point(2, 1)).get.left(Point(3, 1)).get.left(Point(2, 1)).get.vCells mustEqual board_t4.vCells
//    }
//  }
//
//  println("board_t4 After Right-Right-Left-Left: cell3 right, cell5 right, cell6 right")
//  board_t4.print()
//  println()
//
//  "nucleus is" should {
//    Point(1, 1) + " in " + cell1 in {
//      cell1.nucleus mustEqual Point(1, 1)
//    }
//    Point(4, 1) + " in " + cell2 in {
//      cell2.nucleus mustEqual Point(4, 1)
//    }
//  }
//
//
//  "collision" should {
//    val isCollision = PrivateMethod[Boolean]('isCollision)
//    "happen for " + cell1 + " and " + cell1 in {
//      board_t1 invokePrivate isCollision(cell1) mustEqual true
//    }
//    "NOT happen for " + cell1 + " and " + RCell(0, 1, 3, 4, 1) + " and " + cell2 in {
//      board_t1 invokePrivate isCollision(RCell(0, 1, 3, 4, 1)) mustEqual false
//    }
//  }
//
//
//  "allowable board values (of size " + board_t1.dimensions + ")" should {
//    val onBoard = PrivateMethod[Boolean]('onBoard)
//    "are not < than 0" in {
//      board_t1 invokePrivate onBoard(RCell(-1, -1, 2, 2, 1)) mustBe false
//    }
//    "are not >= than the board length" in {
//      board_t1 invokePrivate onBoard(VCell(0, board_t1.dimensions + 1, 3, 2, 1)) mustBe false
//    }
//    "are >= than 0 and < dimensions" in {
//      board_t1 invokePrivate onBoard(RCell(0, -0, 3, 3, 1)) mustBe true
//    }
//  }
//
//  "valid moves for " + cell1 should {
//    val isValidCellState = PrivateMethod[Boolean]('isValidCellState)
//    "are not move up" in {
//      board_t1 invokePrivate isValidCellState(cell1.up) mustBe false
//    }
//    "are not move left" in {
//      board_t1 invokePrivate isValidCellState(cell1.left) mustBe false
//    }
//    "are move down" in {
//      board_t1 invokePrivate isValidCellState(cell1.down) mustBe true
//    }
//    "are move right" in {
//      board_t1 invokePrivate isValidCellState(cell1.right) mustBe true
//    }
//  }
//
//  "valid moves for " + cell2 should {
//    val isValidCellState = PrivateMethod[Boolean]('isValidCellState)
//    "are not move up" in {
//      board_t1 invokePrivate isValidCellState(cell2.up) mustBe false
//    }
//    "are not move left" in {
//      board_t1 invokePrivate isValidCellState(cell2.left) mustBe true
//    }
//    "are move down" in {
//      board_t1 invokePrivate isValidCellState(cell2.down) mustBe true
//    }
//    "are move right" in {
//      board_t1 invokePrivate isValidCellState(cell2.right) mustBe true
//    }
//  }
//
//  cell3 + " doesn't contain" should {
//    "" + RCell(0, 0, 3, 3, 1) in {
//      cell3 contains RCell(0, 0, 3, 3, 1) mustBe false
//    }
//    "" + RCell(3, 0, 6, 3, 1) in {
//      cell3 contains RCell(3, 0, 6, 3, 1) mustBe false
//    }
//    "" + RCell(6, 0, 9, 3, 1) in {
//      cell3 contains RCell(6, 0, 9, 3, 1) mustBe false
//    }
//    "" + RCell(6, 3, 9, 6, 1) in {
//      cell3 contains RCell(6, 3, 9, 6, 1) mustBe false
//    }
//    "" + RCell(0, 6, 3, 9, 1) in {
//      cell3 contains RCell(0, 6, 3, 9, 1) mustBe false
//    }
//    "" + RCell(3, 6, 6, 9, 1) in {
//      cell3 contains RCell(3, 6, 6, 9, 1) mustBe false
//    }
//    "" + RCell(6, 6, 9, 9, 1) in {
//      cell3 contains RCell(6, 6, 9, 9, 1) mustBe false
//    }
//  }
//
//
//  cell3 + "does contain " should {
//    "" + RCell(1, 1, 4, 4, 1) in {
//      cell3 contains RCell(1, 1, 4, 4, 1) mustBe true
//    }
//    "and in reverse 1" in {
//      RCell(1, 1, 4, 4, 1) contains cell3 mustBe true
//    }
//    "" + RCell(2, 1, 5, 4, 1) in {
//      cell3 contains RCell(2, 1, 5, 4, 1) mustBe true
//    }
//    "and in reverse 2" in {
//      RCell(2, 1, 5, 4, 1) contains cell3 mustBe true
//    }
//    "" + VCell(3, 3, 9, 9, 1) in {
//      cell3 contains VCell(3, 3, 9, 9, 1) mustBe true
//    }
//    "and in reverse 3" in {
//      VCell(3, 3, 9, 9, 1) contains cell3 mustBe true
//    }
//    "" + RCell(3, 4, 6, 7, 1) in {
//      cell3 contains RCell(3, 4, 6, 7, 1) mustBe true
//    }
//    "and in reverse 5" in {
//      RCell(3, 4, 6, 7, 1) contains cell3 mustBe true
//    }
//  }
//
//
//  "merging" should {
//    cell1 + "merge " + RCell(0, 1, 3, 4, 1) + " is " + VCell(0, 0, 3, 4, 1, 0) in {
//      cell1 merge RCell(0, 1, 3, 4, 1) mustEqual VCell(0, 0, 3, 4, 1, 0)
//    }
//
//    cell1 + " merge " + RCell(2, 2, 5, 5, 1) + " is " + VCell(0, 0, 5, 5, 1, 0) in {
//      cell1 merge RCell(2, 2, 5, 5, 1) mustEqual VCell(0, 0, 5, 5, 1, 0)
//    }
//
//    cell1 + " merge " + RCell(2, 0, 5, 3, 1) + " merge " + RCell(4, 1, 7, 4, 1) + " is " + VCell(0, 0, 7, 4, 1, 0) in {
//      cell1 merge RCell(2, 0, 5, 3, 1) merge RCell(4, 1, 7, 4, 1) mustEqual VCell(0, 0, 7, 4, 1, 0)
//    }
//
//  }
//
//
////  val rCells_t2b = Vector(cell1, cell2, cell3.down, cell4, cell5, cell6, cell7, cell8, cell9, cell10, cell11, cell12, cell13, cell14)
////  val Adj2 = Vector.fill[Vector[Int]](rCells_t1.length)(Vector.fill[Int](rCells_t1.length)(0))
////  val board_t1b = Board(rCells_t1, Vector(), Adj2)
////  val board_t2b = Board(rCells_t2b, Vector(), Adj2)
////
////  println("board_t1b: Before moving cell3 down")
////  board_t1b.print()
////  println()
////
////  println("board_t2b: After moving cell3 down")
////  board_t2b.print()
////  println()
////
////  cell3 + " moved down is" should {
////    "" + RCell(2, 4, 5, 7, 1) in {
////      board_t1b.down(Point(3, 4)).get.edges mustEqual board_t2b.edges
////      board_t1b.down(Point(3, 4)).get.rCells mustEqual board_t2b.rCells
////      board_t1b.down(Point(3, 4)).get.vCells mustEqual board_t2b.vCells
////
////    }
////  }
////
////  val Adj3 = Vector.fill[Vector[Int]](rCells_t1.length)(Vector.fill[Int](rCells_t1.length)(0))
////  val board_t1c = Board(rCells_t1, Vector(), Adj3)
////
////  val rCells_t2c = Vector(cell1, cell2, cell3.up, cell4, cell5, cell6, cell7, cell8, cell9, cell10, cell11, cell12, cell13, cell14)
////  val Adj3_1 = Vector.fill[Vector[Int]](rCells_t1.length)(Vector.fill[Int](rCells_t1.length)(0))
////
//////  Adj3_1(0)(2) = 1
//////  Adj3_1(2)(0) = 1
//////  Adj3_1(1)(2) = 1
//////  Adj3_1(2)(1) = 1
////  val a4 = Graph(board_t1).updateEdge(0,2,1,Adj3_1)
////  val a5 = Graph(board_t1).updateEdge(1,2,1,a4)
////
////  val board_t2c = Board(rCells_t2c, Vector(VCell(0, 0, 6, 5, 1)), a5)
////
////  println("board_t1c: Before moving cell3 up")
////  board_t1c.print()
////  println()
////
////  println("board_t2c: After moving cell3 up")
////  board_t2c.print()
////  println()
////
////
////  val cell15 = RCell(2, 2, 5, 5, 1)
////  "" + cell3 + " moved up is" should {
////    "" + cell15 in {
////      board_t1c.up(Point(3, 4)).get.edges mustEqual board_t2c.edges
////      board_t1c.up(Point(3, 4)).get.rCells mustEqual board_t2c.rCells
////      board_t1c.up(Point(3, 4)).get.vCells mustEqual board_t2c.vCells
////    }
////  }
////
////
////  val Adj4 = Vector.fill[Vector[Int]](rCells_t1.length)(Vector.fill[Int](rCells_t1.length)(0))
////  val board_t1d = Board(rCells_t1, Vector(), Adj4)
////
////  val rCells_t2d = Vector(cell1, cell2, cell3.left, cell4, cell5, cell6, cell7, cell8, cell9, cell10, cell11, cell12, cell13, cell14)
////
////  val board_t2d = Board(rCells_t2d, Vector(), Adj4)
////
////  println("board_t1d: Before moving cell3 left")
////  board_t1d.print()
////  println()
////
////  println("board_t2d: After moving cell3 left")
////  board_t2d.print()
////  println()
////
////
////  val cell16 = RCell(1, 3, 4, 6, 1)
////  cell3 + " moved left is" should {
////    "" + cell16 in {
////      board_t1d.left(Point(3, 4)).get.edges mustEqual board_t2d.edges
////      board_t1d.left(Point(3, 4)).get.rCells mustEqual board_t2d.rCells
////      board_t1d.left(Point(3, 4)).get.vCells mustEqual board_t2d.vCells
////    }
////  }
////
////
////  val Adj5 = Vector.fill[Vector[Int]](rCells_t1.length)(Vector.fill[Int](rCells_t1.length)(0))
////  val board_t1e = Board(rCells_t1, Vector(), Adj5)
////
////  val rCells_t2e = Vector(cell1, cell2, cell3.right, cell4, cell5, cell6, cell7, cell8, cell9, cell10, cell11, cell12, cell13, cell14)
////  val Adj5_1 = Vector.fill[Vector[Int]](rCells_t1.length)(Vector.fill[Int](rCells_t1.length)(0))
//////  Adj5_1(2)(3) = 1
//////  Adj5_1(3)(2) = 1
////  val a6 = Graph(board_t1).updateEdge(2,3,1,Adj5_1)
////  val board_t2e = Board(rCells_t2e, Vector(VCell(2, 3, 8, 10, 1)), a6)
////
////  println("board_t1e: Before moving cell3 right")
////  board_t1e.print()
////  println()
////
////  println("board_t2e: After moving cell3 left")
////  board_t2e.print()
////  println()
////
////  val cell17 = RCell(3, 3, 6, 6, 1)
////  cell3 + " moved right is" should {
////    "" + cell17 in {
////      board_t1e.right(Point(3, 4)).get.edges mustEqual board_t2e.edges
////      board_t1e.right(Point(3, 4)).get.rCells mustEqual board_t2e.rCells
////      board_t1e.right(Point(3, 4)).get.vCells mustEqual board_t2e.vCells
////    }
////  }
////
////  val Adj6 = Vector.fill[Vector[Int]](11)(Vector.fill[Int](11)(0))
//////  Adj6(1)(0) = 1
//////  Adj6(0)(1) = 1
//////  Adj6(2)(1) = 1
//////  Adj6(1)(2) = 1
////
////  val a7 = Graph(board_t1).updateEdge(1,0,1,Adj6)
////  val a8 = Graph(board_t1).updateEdge(2,1,1,a7)
////
////  val board_t1f = Board(Vector(RCell(0, 0, 3, 3, 1), RCell(2, 2, 5, 5, 1), RCell(4, 4, 7, 7, 1), RCell(7, 0, 10, 3, 1),
////    cell9, cell10, cell11, cell11, cell11, cell11, cell11), Vector(VCell(0, 0, 7, 7, 1)), a8)
////  val board_t1g = Board(Vector(RCell(0, 0, 3, 3, 1), RCell(2, 2, 5, 5, 1), RCell(4, 4, 7, 7, 1), RCell(6, 0, 9, 3, 1),
////    cell9, cell10, cell11, cell11, cell11, cell11, cell11), Vector(VCell(0, 0, 9, 7, 1)), a8)
////
////  println("board_t1f: Before: Move " + RCell(7, 0, 10, 3, 1) + " left to cause virtual-real merge ")
////  board_t1f.print()
////  println()
////
////  println("board_t1g: After: Move " + RCell(7, 0, 10, 3, 1) + " left to cause virtual-real merge ")
////  board_t1g.print()
////
////  "virtual - real merge " should {
////    "happen for " in {
////      board_t1f.left(Point(8, 1)).get.edges mustEqual board_t1g.edges
////      board_t1f.left(Point(8, 1)).get.rCells mustEqual board_t1g.rCells
////      board_t1f.left(Point(8, 1)).get.vCells mustEqual board_t1g.vCells
////    }
////  }
////
////  val Adj7 = Vector.fill[Vector[Int]](12)(Vector.fill[Int](12)(0))
////  //  Adj7(1)(0) = 1
////  //  Adj7(0)(1) = 1
////  //  Adj7(2)(1) = 1
////  //  Adj7(1)(2) = 1
////  val a9 = Graph(board_t1).updateEdge(1,0,1,Adj7)
////  val a10 = Graph(board_t1).updateEdge(2,1,1,a9)
//////
////  val board_t1h = Board(Vector(RCell(0, 6, 3, 9, 1), RCell(2, 8, 5, 11, 1), RCell(4, 10, 7, 13, 1), RCell(6, 3, 9, 6, 1), RCell(9, 5, 12, 8, 1),
////    cell9, cell10, cell11, cell11, cell11, cell11, cell11), Vector(VCell(0, 6, 7, 13, 1)), a10)
////  val board_t1i = Board(Vector(RCell(0, 6, 3, 9, 1), RCell(2, 8, 5, 11, 1), RCell(4, 10, 7, 13, 1), RCell(6, 3, 9, 6, 1), RCell(8, 5, 11, 8, 1),
////    cell9, cell10, cell11, cell11, cell11, cell11, cell11), Vector(VCell(0, 3, 11, 13, 1)), a10)
////
////  println("board_t1h: Before: Move " + RCell(9, 5, 12, 8, 1) + " left to cause virtual-virtual merge")
////  board_t1h.print()
////  println()
////
////  println("board_t1i: After: Move " + RCell(9, 5, 12, 8, 1) + " left to cause virtual-virtual merge")
////  board_t1i.print()
////
////  //  Adj7(3)(4) = 1
////  //  Adj7(4)(3) = 1
////  val a11 = Graph(board_t1).updateEdge(3,4,1,a10)
////
////  val board_t1h2 = Board(Vector(RCell(0, 6, 3, 9, 1), RCell(2, 8, 5, 11, 1), RCell(4, 10, 7, 13, 1), RCell(6, 3, 9, 6, 1), RCell(9, 5, 12, 8, 1),
////    cell9, cell10, cell11, cell11, cell11, cell11, cell11), Vector(VCell(0, 6, 7, 13, 1)), a11)
////  val board_t1i2 = Board(Vector(RCell(0, 6, 3, 9, 1), RCell(2, 8, 5, 11, 1), RCell(4, 10, 7, 13, 1), RCell(6, 3, 9, 6, 1), RCell(8, 5, 11, 8, 1),
////    cell9, cell10, cell11, cell11, cell11, cell11, cell11), Vector(VCell(0, 3, 11, 13, 1)), a11)
////
////  "virtual - virtual merge " should {
////    "happen for " in {
////      board_t1h2.left(Point(10, 6)).get.edges mustEqual board_t1i2.edges
////      board_t1h2.left(Point(10, 6)).get.rCells mustEqual board_t1i2.rCells
////      board_t1h2.left(Point(10, 6)).get.vCells mustEqual board_t1i2.vCells
////    }
////  }
////
////  val Adj8 = Vector.fill[Vector[Int]](12)(Vector.fill[Int](12)(0))
////
////  //  Adj8(1)(0) = 1
//////  Adj8(0)(1) = 1
//////  Adj8(2)(1) = 1
//////  Adj8(1)(2) = 1
////
////  val a12 = Graph(board_t1).updateEdge(1,0,1,Adj8)
////  val a13 = Graph(board_t1).updateEdge(2,1,1,a12)
////
////  val board_t1j = Board(Vector(RCell(0, 6, 3, 9, 1), RCell(2, 8, 5, 11, 1), RCell(4, 10, 7, 13, 1), RCell(6, 3, 9, 6, 1),
////    RCell(8, 5, 11, 8, 1), cell9, cell10, cell11, cell11, cell11, cell11, cell11), Vector(VCell(0, 3, 11, 13, 1)), a13)
////  val board_t1k = Board(Vector(RCell(0, 6, 3, 9, 1), RCell(2, 8, 5, 11, 1), RCell(4, 10, 7, 13, 1), RCell(7, 3, 10, 6, 1),
////    RCell(8, 5, 11, 8, 1), cell9, cell10, cell11, cell11, cell11, cell11, cell11), Vector(VCell(7, 3, 11, 8, 1), VCell(0, 6, 7, 13, 1)), a13)
////
////
////  val a132 = Graph(board_t1).updateEdge(3,4,1,a13)
////
////  val board_t1j2 = Board(Vector(RCell(0, 6, 3, 9, 1), RCell(2, 8, 5, 11, 1), RCell(4, 10, 7, 13, 1), RCell(6, 3, 9, 6, 1),
////    RCell(8, 5, 11, 8, 1), cell9, cell10, cell11, cell11, cell11, cell11, cell11), Vector(VCell(0, 3, 11, 13, 1)), a132)
////  val board_t1k2 = Board(Vector(RCell(0, 6, 3, 9, 1), RCell(2, 8, 5, 11, 1), RCell(4, 10, 7, 13, 1), RCell(7, 3, 10, 6, 1),
////    RCell(8, 5, 11, 8, 1), cell9, cell10, cell11, cell11, cell11, cell11, cell11), Vector(VCell(7, 3, 11, 8, 1), VCell(0, 6, 7, 13, 1)), a132)
////
////
////  println("board_t1j: Before: Move " + RCell(6, 3, 9, 6, 1) + " right to cause virtual to v-v split")
////  board_t1j2.print()
////  println()
////
////  println("board_t1k: After: Move " + RCell(6, 3, 9, 6, 1) + " right to cause virtual to v-v split")
////  board_t1k2.print()
////
////  "virtual break up merge " should {
////    "happen for " in {
////      board_t1j2.right(Point(7, 4)).get.edges mustEqual board_t1k2.edges
////      board_t1j2.right(Point(7, 4)).get.rCells mustEqual board_t1k2.rCells
////      board_t1j2.right(Point(7, 4)).get.vCells mustEqual board_t1k2.vCells
////    }
////  }
////
////  val Adj9 = Vector.fill[Vector[Int]](14)(Vector.fill[Int](14)(0))
////
//////  Adj9(2)(3) = 1
//////  Adj9(3)(2) = 1
//////  Adj9(3)(4) = 1
//////  Adj9(4)(3) = 1
//////  Adj9(5)(6) = 1
//////  Adj9(6)(5) = 1
////
////  val a14 = Graph(board_t1).updateEdge(2,3,1,Adj9)
////  val a15 = Graph(board_t1).updateEdge(3,4,1,a14)
////  val a16 = Graph(board_t1).updateEdge(5,6,1,a15)
////
////
////  val board_t4a = Board(Vector(cell1,cell2,cell3.right,cell4,cell5.right,cell6.right.right,cell7,cell8,cell9,
////                                   cell10,cell11,cell12,cell13,cell14),Vector(VCell(8,0,12,5,1),VCell(3,3,8,10,1)), a16)
////
////  val rCells_t4b = Vector(cell1,cell2,cell3.right,cell4,cell5.right,cell6.right,cell7,cell8.capture(1).asInstanceOf[RCell],cell9,cell10,cell11,cell12,cell13,cell14)
////  val board_t4b = Board(rCells_t4b, Vector(VCell(3,0,12,12,1)), a16)
////
////
////  println("board_t4b: Before: V-V Capture")
////  board_t4a.print()
////  println()
////
////  println("board_t4b: After V-V Capture")
////  board_t4a.left(Point(9, 1)).get.print()
////
////  "virtual capture " should {
////    "happen for " in {
////      board_t4a.left(Point(9, 1)).get.edges mustEqual board_t4b.edges
////      board_t4a.left(Point(9, 1)).get.rCells mustEqual board_t4b.rCells
////      board_t4a.left(Point(9, 1)).get.vCells mustEqual board_t4b.vCells
////    }
////  }
////
////  val Adj10 = Vector.fill[Vector[Int]](14)(Vector.fill[Int](14)(0))
//////  Adj10(0)(2) = 1
//////  Adj10(2)(0) = 1
//////  Adj10(3)(4) = 1
//////  Adj10(4)(3) = 1
//////  Adj10(5)(6) = 1
//////  Adj10(6)(5) = 1
//////  Adj10(12)(13) = 1
//////  Adj10(13)(12) = 1
////
////  val a17 = Graph(board_t1).updateEdge(0,2,1,Adj10)
////  val a18 = Graph(board_t1).updateEdge(3,4,1,a17)
////  val a19 = Graph(board_t1).updateEdge(5,6,1,a18)
////  val a20 = Graph(board_t1).updateEdge(12,13,1,a19)
////
////  val board_t5a = Board(Vector(RCell(1,0,4,3,1), RCell(0,3,3,6,2),RCell(4,2,7,5,2),RCell(7,5,10,8,2),RCell(6,6,9,9,2),RCell(0,7,3,10,2),RCell(2,9,5,12,2),
////                        cell8,cell9,cell10,cell11,cell12,cell13.right.down,cell14), Vector(VCell(0,7,5,12,2), VCell(6,5,10,9,2), VCell(13,12,20,18,2)), a20)
////
////
////  val board_t5b = Board(Vector(RCell(2,0,5,3,1), RCell(0,3,3,6,1),RCell(4,2,7,5,1),RCell(7,5,10,8,1),RCell(6,6,9,9,1),RCell(0,7,3,10,1),RCell(2,9,5,12,1),
////                        RCell(9,9,12,12,1),cell9,cell10,cell11,cell12,cell13.right.down,cell14), Vector(VCell(0,0,12,12,1),VCell(13,12,20,18,2)), a20)
////
////
////  println("board_t5a: Before: R-R V-R V-V")
////  board_t5a.print()
////  println()
////
////  println("board_t5a: After R-R V-R V-V")
////  board_t5a.right(Point(2, 1)).get.print()
////
////  "R-R...... " should {
////    "should give one VCell for team 1" in {
////      board_t5a.right(Point(2, 1)).get.edges mustEqual board_t5b.edges
////      board_t5a.right(Point(2, 1)).get.rCells mustEqual board_t5b.rCells
////      board_t5a.right(Point(2, 1)).get.vCells mustEqual board_t5b.vCells
////
////    }
////  }
////
////
////  val Adj11 = Vector.fill[Vector[Int]](14)(Vector.fill[Int](14)(0))
//////  Adj11(3)(4) = 1
//////  Adj11(4)(3) = 1
//////  Adj11(5)(6) = 1
//////  Adj11(6)(5) = 1
//////  Adj11(12)(13) = 1
//////  Adj11(13)(12) = 1
////
////  val a21 = Graph(board_t1).updateEdge(3,4,1,Adj11)
////  val a22 = Graph(board_t1).updateEdge(5,6,1,a21)
////  val a23 = Graph(board_t1).updateEdge(12,13,1,a22)
////
////
////  val board_t6a = Board(Vector(RCell(1,0,4,3,2), RCell(0,3,3,6,2),RCell(4,2,7,5,1),RCell(7,5,10,8,2),RCell(6,6,9,9,2),RCell(0,7,3,10,2),RCell(2,9,5,12,2),
////    cell8,cell9,cell10,cell11,cell12,cell13.right.down,cell14), Vector(VCell(0,7,5,12,2), VCell(6,5,10,9,2), VCell(13,12,20,18,2)), a23)
////
////
////  val board_t6b = Board(Vector(RCell(1,0,4,3,2), RCell(0,3,3,6,1),RCell(4,3,7,6,1),RCell(7,5,10,8,1),RCell(6,6,9,9,1),RCell(0,7,3,10,1),RCell(2,9,5,12,1),
////    RCell(9,9,12,12,1),cell9,cell10,cell11,cell12,cell13.right.down,cell14), Vector(VCell(0,3,12,12,1),VCell(13,12,20,18,2)), a23)
////
////
////  println("board_t6a: Before: R-V V-R V-V")
////  board_t6a.print()
////  println()
////
////  println("board_t6a: After R-V V-R V-V")
////  board_t6a.down(Point(5, 3)).get.print()
////
////  "R-V......... " should {
////    "should give one VCell for team 1" in {
////      board_t6a.down(Point(5, 3)).get.edges mustEqual board_t6b.edges
////      board_t6a.down(Point(5, 3)).get.rCells mustEqual board_t6b.rCells
////      board_t6a.down(Point(5, 3)).get.vCells mustEqual board_t6b.vCells
////
////    }
////  }
////
////  println("====================== Performance ======================")
////  println("")
////
////
////  val t1 = time{
////    board_t1j.right(Point(7, 4))
////  }
////  println("Time to v-v split: " + t1)
////
////
////  val t2 = time{
////    board_t1h.left(Point(10,6))
////  }
////  println("Time to v-v merge: " + t2)
////
////
////  val t3 = time{
////    board_t1f.left(Point(8,1))
////  }
////  println("Time to v-v merge: " + t3)
////
////  val t4 = time{
////    board_t1e.right(Point(3,4))
////  }
////  println("Time move cell:" + t4)
////
////  val t5 = time{
////    board_t4.right(Point(1, 1)).get.right(Point(2,1)).get.left(Point(3,1)).get.left(Point(2, 1))
////  }
////  println("Time move cell 4 times:" + t5)
////  println("")
////  println("=========================================================")
//}