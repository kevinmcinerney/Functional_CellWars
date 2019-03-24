package model

import game.{Cell, RCell}

/**
  * The Graph
  * @param board the board to map onto graph
  */
case class Graph() {


  /**
    * Find Connected Components
    * @param rCellsMovedWithEdges list of of all updated cells
    * @param skipTraversal when free cells move away from others
    * @param mover_idx index of moved rCell
    * @return new updated Board
    */
  def update(rCellsMovedWithEdges: Vector[RCell], skipTraversal: Boolean, inVCell: Boolean, mover_idx: Int): Option[Board] =  {

    if(skipTraversal && !inVCell){

      None

    }else{

      val (free, connected) = fdfs(rCellsMovedWithEdges).partition((p: Vector[RCell]) => p.size == 1)

      val redVCells = reduceVTrees(connected, mover_idx)

      Some(Board(free.flatten, redVCells))
    }
  }


  /**
    * Forest Depth First Search
    * @param rCellsMovedWithEdges list of of all updated cells
    * @return list of spanning trees consisting of connected RCells
    */
  private def fdfs(rCellsMovedWithEdges: Vector[RCell]): Vector[Vector[RCell]] = {
    rCellsMovedWithEdges.foldLeft(Vector[Vector[RCell]]()) {
      (acc, next) => {
        if(acc contains next) acc
        else acc :+ dfs(next, rCellsMovedWithEdges)
      }
    }
  }


  /**
    * Depth First Search
    * @param current current cell being visited
    * @param rCellsMovedWithEdges list of of all updated cells
    * @param acc accumulation of already visited cells
    * @return spanning tree consisting of connected rCells
    */
  private def dfs(current: RCell, rCellsMovedWithEdges: Vector[RCell], acc: Vector[RCell] = Vector()): Vector[RCell] = {
    current.edges.foldLeft(acc) {
      (results, next) =>
        if (results.contains(rCellsMovedWithEdges(next))) results
        else dfs(rCellsMovedWithEdges(next), rCellsMovedWithEdges, results :+ current)
    } :+ current
  }


  /**
    * Depth First Search
    * @param vTrees  list of spanning trees consisting of connected RCells
    * @param mover_idx index of moved cell
    * @return list of VCells formed from spanning trees of RCells
    */
  private def reduceVTrees(vTrees: Vector[Vector[RCell]], mover_idx: Int): Vector[Cell] =  {
    vTrees
      .map((ys: Vector[RCell]) => ys
        .reduce{
          (a: Cell, b: Cell) => if (a.id  == mover_idx) a merge b else b merge a
        })
  }


}

