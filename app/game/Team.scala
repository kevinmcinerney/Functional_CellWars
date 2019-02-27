//package game
//
//import model.Board
//
///**
//  * Created by kevin on 30/04/17.
//  */
//
//case class Team(cells: List[Cell]) {
//
////  def size = cells.size
////
////  //def allPairs(n: Int): List[List[Cell]] = cells combinations(n) toList
////
////  def realCells: List[RCell] = {
////    cells.collect { case a: RCell => a }
////  }
////
////  def virtualCells: List[VCell] = {
////    cells.collect { case a: VCell => a }
////  }
////
////  def contains(nucleus: Point): Boolean = {
////    realCells.exists(_.nucleus == nucleus)
////  }
//
//}
//
//object Team {
//
//  def apply(cell : Cell*): Team =
//    Team(cell.toList)
//}
