package model

import game.{Cell, RCell, VCell}

import scala.collection.mutable.ListBuffer
import scala.util.Try

/**
  * The Graph
  * @param board the board to map onto graph
  */
case class Graph(board: Board)
{

  val adj = board.edges.map(_.clone)

  val vertexList = board.rCells.clone()

  val vCells = board.vCells.clone()

  val V = vertexList.length

  /**
    * Add edges to graph representing connections between RCells
    * @param c1_idx index of RCell in [[Graph.vertexList]]
    * @param c2_idx index of RCell in [[Graph.vertexList]]
    */
  def addEdge(c1_idx: Int, c2_idx: Int): Unit = {
    adj(c1_idx)(c2_idx) = 1
    adj(c2_idx)(c1_idx) = 1
  }

  /**
    * Add edges to graph representing connections between RCells
    * @param c1_idx index of RCell in [[Graph.vertexList]]
    * @param c2_idx index of RCell in [[Graph.vertexList]]
    */
  def delEdge(c1_idx: Int, c2_idx: Int): Unit = {
    adj(c1_idx)(c2_idx) = 0
    adj(c2_idx)(c1_idx) = 0
  }


  /**
    * Add edges to graph representing connections between RCells
    * @param rCell RCell to evalute
    * @return true if rCell is inside of one of VCells
    */
  def isInsideVCell(rCell: RCell): Boolean = {
    vCells.exists(v => v contains rCell)
  }

  /**
    * Add edges to graph representing connections between RCells
    * @param idx index of RCell in [[Graph.vertexList]]
    * @return Option[GraphUpdateResult]
    */
  def update(idx: Int): Option[Board] = {

    // del edges with children
    var deleted = false
    for((con, child_idx) <- adj(idx).zipWithIndex if con == 1) {
      delEdge(child_idx, idx)
      deleted = true
    }

    // add edges
    var added = false
    for(child_idx <- vertexList.indices if (vertexList(child_idx) contains vertexList(idx)) && (child_idx != idx)) {
      addEdge(child_idx, idx)
      added = true
    }

    if(!deleted && !added && !isInsideVCell(vertexList(idx))){
      None
    }
    else {
      val (real, virtual) = FDFS(idx).partition(_.length == 1)
      //println("virtual: " + virtual)
      val redVCells = reduceVTrees(virtual)
      //println("redVCells: " + redVCells)
      Some(Board(real.flatten, redVCells, adj))
    }
  }

  /**
    * Forest Depth First Search
    * @param idx index of RCell to start traversal of [[Graph.vertexList]]
    * @return list of spanning trees consisting of connected RCells
    */
  def FDFS(idx: Int): ListBuffer[ListBuffer[RCell]] = {

    val spanningTrees: ListBuffer[ListBuffer[RCell]] = ListBuffer()

    for (i <- Range(idx, V + idx).map(_ % V)) {
      if (!vertexList(i).visited) {
        //println("visited: " + i)
        vertexList(i).visited = true
        val spanningTree = DFS(i)
        spanningTrees += spanningTree
      }
    }
    vertexList.foreach { v => v.visited = false }
    spanningTrees
  }

  /**
    * Depth First Search
    * @param vTrees  list of spanning trees consisting of connected RCells
    * @return list of VCells formed from spanning trees of RCells
    */
  private def reduceVTrees(vTrees: ListBuffer[ListBuffer[RCell]]): ListBuffer[Cell] = {
    vTrees
      .map((ys: ListBuffer[RCell]) => ys
        .reduce((a: Cell, b: Cell) => a merge b))
  }

  /**
    * Depth First Search
    * @param idx index of RCell to start traversal of [[Graph.vertexList]]
    * @return spanning tree of connected RCells starting from idx
    */
  private def DFS(idx: Int): ListBuffer[RCell] = {
    // Mark the current node as visited and print it
    val stack = new scala.collection.mutable.Stack[Int]
    stack.push(idx)

    val spanningTree: ListBuffer[RCell] = ListBuffer(vertexList(idx))

    while (stack.nonEmpty) {
      val idx = getUnvisitedVertex(stack.top)
      idx match {
        case Some(x) if x != V => {
          vertexList(x).visited = true
          //println(" and " + x)
          spanningTree += vertexList(x)
          stack.push(x)
        }
        case Some(x) if x == V => stack.pop()
        case None => stack.pop()
      }
    }
    //println()
    spanningTree
  }

  /**
    * Find unvisited vertices
    * @param idx index of RCell to start traversal of [[Graph.vertexList]]
    * @return int of unvisited RCell in [[Graph.vertexList]]
    */
  private def getUnvisitedVertex(idx: Int): Option[Int] = {
    var i = 0
    while (i < V && !(adj(idx)(i) == 1 && !vertexList(i).visited)) {
      i += 1
    }
    Some(i)
  }
}

