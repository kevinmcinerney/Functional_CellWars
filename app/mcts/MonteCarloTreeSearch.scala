package mcts

import breeze.linalg.{max, min}
import game.RandomGame
import model.Board

import scala.collection.mutable.ListBuffer
import scala.collection.parallel.ParSeq
import scala.util.Random


/**
  * Represents a studied node.
  * @param parent the parent node
  * @param subNodes the sub nodes
  */
class Node(val state: State, val parent: Node = null, var subNodes: Seq[Node] = Seq.empty){

  def this(node: Node) = {
    this(node.state, node.parent)
  }
}

class Tree(var root: Node)

/**
  * Represents a studied node.
  * @param board the [[model.Board]]
  * @param playerNo player number
  * @param visitCount the number of visit
  * @param winScore the score
  */
class State(val board: Board, var playerNo: Int, var visitCount: Int = 0, var winScore: Double = 0){

  def opponent = 3 - playerNo

  def getAllPossibleStates: Seq[State] = {
    val playerCells = board.rCells.filter(_.marker == playerNo)
    var state = ListBuffer[State]()

    for(c <- playerCells) {
      state  += new State(board.up(c.nucleus).get, opponent)
      state  += new State(board.down(c.nucleus).get, opponent)
      state  += new State(board.left(c.nucleus).get, opponent)
      state  += new State(board.right(c.nucleus).get, opponent)
    }
    state
  }

  def randomPlay: Int = {
    RandomGame.randomPlay(board)
  }


}

object UCT {

  def uctValue(totalVisit: Int, nodeWinScore: Double, nodeVisit: Int): Double = {
    if(nodeVisit == 0) {
      Int.MaxValue
    }
    else{
      (nodeWinScore / nodeVisit) + 1.41 * Math.sqrt(Math.log(totalVisit) / nodeVisit)
    }
  }

  def findBestNodeWithUCT(node: Node): Node = {
    val parentVisit = node.state.visitCount
    node.subNodes.maxBy(n => uctValue(parentVisit, n.state.winScore, n.state.visitCount))
  }
}

/**
  * The Monte Carlo tree search implementation.
  */
class MonteCarloTreeSearch {

  /** The random instance. */
  val random: Random = new Random(System.currentTimeMillis())

  def bestMove(p_board: Board, p_playerNo: Int, results: ParSeq[Seq[Node]]): Int = {

    results.toList.indices.foreach(core => {results(core).indices.foreach(node => {results(core)(node).state.board.edges.foreach(row => {row.foreach(print(_)); println()}); println(results(core)(node).state.board.vCells);println("NEXT BOARD")}); println();println("NEXT CORE")})

    val team = p_board.rCells.filter(_.marker == p_playerNo)
    val visits = new Array[Int](team.length*4)
    val scores = new Array[Double](team.length*4)
    for(core <- 0 until results.length;
        res  <- 0 until team.length*4){
        scores(res) += results(core)(res).state.winScore
        visits(res) += results(core)(res).state.visitCount
      }
    val tuples = scores zip visits
    val norm = normalize(tuples.map(tup => tup._1 / tup._2))
    var r = ListBuffer[(String, Int, String)]()
    norm.indices.foreach(idx => {
      idx % 4 match {
        case 0 => r += (("Up: Cell  ", team(idx/4).id, repeatChar('*', norm(idx).toInt)))
        case 1 => r += (("Down: Cell  ", team(idx/4).id, repeatChar('*', norm(idx).toInt)))
        case 2 => r += (("Left: Cell    ", team(idx/4).id, repeatChar('*', norm(idx).toInt)))
        case 3 => r += (("Right: Cell ", team(idx/4).id, repeatChar('*', norm(idx).toInt)))
      }
    })
    r.sortBy(_._3.length).foreach(r => println(r._1,r._2, r._3))
    tuples.indexOf(tuples.maxBy(tup => tup._1 / tup._2))
  }

  /**
    * Find best next move from the specified board and the max duration.<br/>
    * Implements the Monte Carlo Tree Search
    * @param uctsTime the max duration
    * @return the best board
    */
  def findNextMove(board: Board, playerNo:Int, uctsTime: Int): Seq[Node] = {

    val end = System.currentTimeMillis() + uctsTime
    val state = new State(board, playerNo)
    val rootNode = new Node(state)


    while (System.currentTimeMillis() < end) {

      val promiseNode: Node = selectPromiseNode(rootNode)

      if (promiseNode.state.board.boardStatus == Board.IN_PROGRESS) {
        expandNode(promiseNode)
      }

      var nodeToExplore: Node = promiseNode

      if (promiseNode.subNodes.nonEmpty) {
        nodeToExplore = promiseNode.subNodes(random.nextInt(promiseNode.subNodes.size))
      }

      val playoutResult: Int = simulateRandomPlayout(nodeToExplore)

      backPropogation(nodeToExplore, playoutResult)

    }
    rootNode.subNodes
  }

  /**
    * UCT Upper Confidence Bounds Applied to Trees
    * Select the promise node = the node to study
    * @param rootNode the root node
    * @return the promise node
    */
  def selectPromiseNode(rootNode: Node): Node = {
    var node = rootNode
    while (node.subNodes.nonEmpty) {
      node = UCT.findBestNodeWithUCT(node)
    }
    node
  }



  /**
    * Expand the specified node.
    * @param node the specified node
    */
  def expandNode(node: Node): Unit = {
    val possibleStates = node.state.getAllPossibleStates
    node.subNodes =
    for(newState <- possibleStates) yield new Node(newState,node)
  }

  /**
    * Simulate a random game from the specified node.
    * @param node the specified node
    * @return the score of the random game
    */
  def simulateRandomPlayout(node: Node): Int = {
    val tempNode = new Node(node)
    val tempState = tempNode.state
    val boardStatus = tempState.board.boardStatus
    if(boardStatus == tempState.opponent){
      tempNode.parent.state.winScore = Int.MinValue
      boardStatus
    }
    else{
      tempState.randomPlay
    }
  }

  /**
    * Update tree node (nbVisit and score) from the specified node with the specified game result
    * @param nodeToExplore the specified node
    * @param playerNo player number
    */
  def backPropogation(nodeToExplore: Node, playerNo: Int): Unit = {
    var tempNode = nodeToExplore
    while (tempNode != null) {
      tempNode.state.visitCount += 1
      if (tempNode.state.playerNo == playerNo) {
          tempNode.state.winScore += Board.WIN_SCORE
        }
      tempNode = tempNode.parent
      }
  }

  def repeatChar(c: Char, n: Int): String = c.toString * n

  def normalize(xs: Seq[Double]) = {
    xs.map(x => (100 * (x - min(xs))) / (max(xs) - min(xs)))
  }

}

