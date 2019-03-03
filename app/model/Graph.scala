package model

import game.{Cell, RCell, VCell}

import scala.collection.mutable.ListBuffer
import scala.util.Try

/**
  * Created by kevin on 19/02/19.
  */


case class Graph(board: Board) // No. of vertices)
{

  val adj = board.edges.clone()

  val vertexList = board.rCells.clone()

  val vCells = board.vCells

  val V = vertexList.length


  //Function to add an edge between two rCell Vertices
  def addEdge(c1_idx: Int, c2_idx: Int): Unit = {
    adj(c1_idx)(c2_idx) = 1
    adj(c2_idx)(c1_idx) = 1
  }

  //Function to add an edge between two rCell Vertices
  def delEdge(c1_idx: Int, c2_idx: Int): Unit = {
    adj(c1_idx)(c2_idx) = 0
    adj(c2_idx)(c1_idx) = 0
  }

  def getIdx(cell: RCell) = vertexList.indexWhere(_.nucleus == cell.nucleus)

  def isInsideVCell(rCell: RCell): Boolean = {
    vCells.exists(v => v contains rCell)
  }

  def update(idx: Int): Option[GraphUpdateResult] = {

    // find children of moved cell
    val children: Array[Int] = adj(idx).zipWithIndex.filter(_._1 == 1).map(p => p._2)

    // del edges with children
    children.foreach(child => delEdge(child, idx))

    // find edges
    val pairs = vertexList zip Array.fill(vertexList.length)(vertexList(idx))
    val merge = pairs.filter(p => (p._1 contains p._2) && p._1.nucleus != p._2.nucleus)

    if (children.nonEmpty || merge.nonEmpty || isInsideVCell(vertexList(idx)) ) {

      // add edges
      val index = merge.map(p => (getIdx(p._1), getIdx(p._2)))
      merge.foreach(p => index.foreach(i => addEdge(i._1, i._2)))

      val forest = FDFS(idx)

      val redVCells = reduceVTrees(forest)

      Some(GraphUpdateResult(adj = adj,
        vCells = redVCells,
        mergedAlready = forest))
    }
    else {
      None
    }
  }

  def FDFS(idx: Int): ListBuffer[ListBuffer[RCell]] = {

    val spanningTrees: ListBuffer[ListBuffer[RCell]] = ListBuffer()

    for (i <- Range(idx, V + idx).map(_ % V)) {
      if (!vertexList(i).visited) {
        vertexList(i).visited = true
        val spanningTree = DFS(i)
        if (spanningTree.length > 1) spanningTrees += spanningTree

      }
    }

    vertexList.foreach { v => v.visited = false }

    spanningTrees
      .filter((xs: ListBuffer[RCell]) => xs.length > 1)

  }

  private def reduceVTrees(vTrees: ListBuffer[ListBuffer[RCell]]) = {
    vTrees
      .map((ys: ListBuffer[RCell]) => ys
        .reduce((a: Cell, b: Cell) => a merge b))
  }

  // A DFS used by DFS
  private def DFS(i: Int): ListBuffer[RCell] = {
    // Mark the current node as visited and print it
    val stack = new scala.collection.mutable.Stack[Int]
    stack.push(i)
    //2
    // 5
    // kevin
    // println("Visited: " + vertexList(i))

    val spanningTree: ListBuffer[RCell] = ListBuffer(vertexList(i))

    while (stack.nonEmpty) {
      val idx = getUnvisitedVertex(stack.top)
      idx match {
        case Some(x) if x != V => {
          vertexList(x).visited = true
          //println(" and " + vertexList(x))
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

  private def getUnvisitedVertex(v: Int): Option[Int] = {
    var i = 0
    while (i < V && !(adj(v)(i) == 1 && !vertexList(i).visited)) {
      i += 1
    }
    Some(i)
  }
}

