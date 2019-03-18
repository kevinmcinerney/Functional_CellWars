package model

import game.{Cell, RCell, VCell}

import scala.collection.mutable.ListBuffer

/**
  * The Graph
  * @param board the board to map onto graph
  */
case class Graph(board: Board)
{

  val b = board

  val adj = b.edges

  val vertexList = b.rCells

  val vCells = b.vCells

  val V = vertexList.length

  /**
    * Add edges to graph representing connections between RCells
    * @param c1_idx index of RCell in [[Graph.vertexList]]
    * @param c2_idx index of RCell in [[Graph.vertexList]]
    */
  private def addEdge(p_adj:Vector[Vector[Int]], c1_idx: Int, c2_idx: Int): Vector[Vector[Int]] = synchronized {
    val upd1 = p_adj.updated(c1_idx, p_adj(c1_idx).updated(c2_idx, 1))
    upd1.updated(c2_idx, p_adj(c2_idx).updated(c1_idx, 1))

  }

  /**
    * Add edges to graph representing connections between RCells
    * @param c1_idx index of RCell in [[Graph.vertexList]]
    * @param c2_idx index of RCell in [[Graph.vertexList]]
    */
  private def delEdge(p_adj:Vector[Vector[Int]], c1_idx: Int, c2_idx: Int): Vector[Vector[Int]] = synchronized {
    val upd1 = p_adj.updated(c1_idx, p_adj(c1_idx).updated(c2_idx, 0))
    upd1.updated(c2_idx, p_adj(c2_idx).updated(c1_idx, 0))
  }


  /**
    * Add edges to graph representing connections between RCells
    * @param rCell RCell to evalute
    * @return true if rCell is inside of one of VCells
    */
  private def isInsideVCell(rCell: RCell): Boolean = synchronized {
    vCells.exists(v => v contains rCell)
  }

  /**
    * Add edges to graph representing connections between RCells
    * @param idx index of RCell in [[Graph.vertexList]]
    * @return Option[GraphUpdateResult]
    */
  def update(idx: Int): Option[Board] = synchronized {

    // del edges with children
    var tempAdj = adj
    var deleted = false
    for((con, child_idx) <- adj(idx).zipWithIndex if con == 1) {
      synchronized{
        tempAdj = delEdge(adj, child_idx, idx)
        deleted = true
      }
    }

    // add edges
    var added = false
    for(child_idx <- vertexList.indices if (vertexList(child_idx) contains vertexList(idx)) && (child_idx != idx)) {
      synchronized{
        tempAdj = addEdge(tempAdj, child_idx, idx)
        added = true
      }

    }

    if(!deleted && !added && !isInsideVCell(vertexList(idx))){
      None
    }
    else {
      val (real, virtual) = FDFS(tempAdj,idx).partition(_.length == 1)
//      println("  " + (0 to vertexList.length-1).mkString(""))
//      adj.indices.foreach(row => {print(row + " "); adj(row).foreach(i => print(i)); println()})
//      virtual.foreach(list => {list.foreach(v => print(v.id + "=>")); print("  ")})
//      println()
//      println()
      val redVCells = reduceVTrees(virtual)
      //Board(vertexList, redVCells, adj).print()
      Some(Board(real.flatten, redVCells, adj))
    }
  }

  /**
    * Forest Depth First Search
    * @param idx index of RCell to start traversal of [[Graph.vertexList]]
    * @return list of spanning trees consisting of connected RCells
    */
  private def FDFS(tempAdj: Vector[Vector[Int]], idx: Int): Vector[Vector[RCell]] = synchronized {

    //var spanningTrees: Vector[Vector[RCell]] = Vector()
    val spanningTrees: ListBuffer[Vector[RCell]] = ListBuffer()

    for (i <- Range(idx, V + idx).map(_ % V)) {
      if (!vertexList(i).visited) {
        //println("visited: " + i)
        vertexList(i).visited = true
        val spanningTree = DFS(tempAdj, i)
//        spanningTrees = spanningTrees :+ spanningTree
        synchronized{
          spanningTrees += spanningTree
        }

      }
    }
    synchronized{
      vertexList.foreach { v => v.visited = false }
    }

    spanningTrees.toVector
  }

  /**
    * Depth First Search
    * @param vTrees  list of spanning trees consisting of connected RCells
    * @return list of VCells formed from spanning trees of RCells
    */
  private def reduceVTrees(vTrees: Vector[Vector[RCell]]): Vector[Cell] = synchronized {
    vTrees
      .map((ys: Vector[RCell]) => ys
        .reduce((a: Cell, b: Cell) => a merge b))
  }

  /**
    * Depth First Search
    * @param idx index of RCell to start traversal of [[Graph.vertexList]]
    * @return spanning tree of connected RCells starting from idx
    */
  private def DFS(tempAdj: Vector[Vector[Int]], idx: Int): Vector[RCell] = synchronized {
    // Mark the current node as visited and print it
    val stack = new scala.collection.mutable.Stack[Int]
    stack.push(idx)

    //var spanningTree: Vector[RCell] = Vector(vertexList(idx))
    val spanningTree: ListBuffer[RCell] = ListBuffer(vertexList(idx))

    while (stack.nonEmpty) {
      val idx = getUnvisitedVertex(tempAdj,stack.top)
      idx match {
        case Some(x) if x != V => {
          vertexList(x).visited = true
          //println(" and " + x)
          //spanningTree = spanningTree :+ vertexList(x)
          synchronized{
            spanningTree += vertexList(x)
          }
          stack.push(x)
        }
        case Some(x) if x == V => stack.pop()
        case None => stack.pop()
      }
    }
    spanningTree.toVector
  }

  /**
    * Find unvisited vertices
    * @param idx index of RCell to start traversal of [[Graph.vertexList]]
    * @return int of unvisited RCell in [[Graph.vertexList]]
    */
  private def getUnvisitedVertex(tempAdj: Vector[Vector[Int]],idx: Int): Option[Int] = synchronized {
    var i = 0
    while (i < V && !(tempAdj(idx)(i) == 1 && !vertexList(i).visited)) {
      synchronized{
        i += 1
      }

    }
    Some(i)
  }
}

