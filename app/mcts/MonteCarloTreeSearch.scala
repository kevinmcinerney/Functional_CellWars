package mcts

import game.{Cell, RCell}
import model.Board

import scala.collection.mutable.ListBuffer
import scala.util.Random


/**
  * Represents a studied node.
  * @param board the [[model.Board]]
  * @param parent the parent node
  * @param subNodes the sub nodes
  * @param nbVisit the number of visit
  * @param score the score
  */
class Node(val board: Board,
           var player: Int,
           val parent: Node = null,
           var subNodes: Seq[Node] = Seq.empty,
           var nbVisit: Int = 0,
           var score: Double = 0)

/**
  * The Monte Carlo tree search implementation.
  */
object MonteCarloTreeSearch {

  /** The random instance. */
  val random: Random = new Random(System.currentTimeMillis())

  /**
    * Find best next move from the specified board and the max duration.<br/>
    * Implements the Monte Carlo Tree Search
    * @param board the specified board
    * @param uctsTime the max duration
    * @return the best board
    */
  def findNextMove(board: Board, uctsTime: Int, player: Int): Board = {
    val end = System.currentTimeMillis() + uctsTime
    val rootNode = new Node(board, player)
    var nbTurn = 0
    while (System.currentTimeMillis() < end) {
      //println("starting..")
      nbTurn += 1
      val promiseNode = selectPromiseNode(rootNode)
      //println("A..")
      if (promiseNode.subNodes.isEmpty) {
        expandNode(promiseNode)
      }
      //println("B..")
      var nodeToExplore = promiseNode
      if (promiseNode.subNodes.nonEmpty) {
        nodeToExplore = promiseNode.subNodes(random.nextInt(promiseNode.subNodes.size))
      }
      //println("C..")
//      nodeToExplore.board.print()
      //val t1: Long = System.currentTimeMillis()
      val playoutResult = simulateRandomPlayout(nodeToExplore)
      //val t2: Long = System.currentTimeMillis()
      //println("Game took " + t2.-(t1))

      //println("playoutResult " + playoutResult)
      backPropogation(nodeToExplore, playoutResult)
      //println("playoutResult: " + playoutResult)
    }
    val bestNode = rootNode.subNodes.maxBy(n => n.score.toDouble / n.nbVisit)
    //logTree(rootNode)
    //Console.err.println("rootNode.subNodes :\n\t" + rootNode.subNodes.map(s => (s.score, s.nbVisit)).mkString("\n\t"))
    Console.err.println("bestNode.score = " + bestNode.score + " | Visited " + bestNode.nbVisit + " from a total of " + nbTurn + " = " + bestNode.score.toDouble / bestNode.nbVisit)
    rootNode.subNodes.foreach(n => println(n.score / n.nbVisit))
    println()
    bestNode.board
  }

  /**
    * UCT Upper Confidence Bounds Applied to Trees
    * Select the promise node = the node to study
    * @param rootNode the root node
    * @return the promise node
    */
  def selectPromiseNode(rootNode: Node): Node = {
    //println("entering spn...")
    var node = rootNode
    while (node.subNodes.nonEmpty) {
      val parentVisitScore = math.log(node.nbVisit)
      node = node.subNodes.maxBy { subNode =>
        if (subNode.nbVisit == 0) {
          Int.MaxValue.toDouble
        } else {
          (1.41D * math.sqrt(parentVisitScore / subNode.nbVisit)) + (subNode.score.toDouble / subNode.nbVisit)
        }
      }
    }
    //println("leaving spn...")
    node
  }

  /**
    * Expand the specified node.
    * @param node the specified node
    */
  def expandNode(node: Node): Unit = {
    //println("entering en..")
    node.subNodes = node.board.getAllPossibleNextBoard(node.player)
      .map{ board => {
          new Node(board,
            3 - node.player,
            node)
        }
      }
    //println("exiting en..")
  }

  /**
    * Simulate a random game from the specified node.
    * @param node the specified node
    * @return the score of the random game
    */
  def simulateRandomPlayout(node: Node): Int = {
    if (node.board.boardStatus == 2) {
      node.parent.score = Int.MinValue
      -1
    } else {
      node.board.playout(node.board)
    }
  }

  /**
    * Update tree node (nbVisit and score) from the specified node with the specified game result
    * @param node the specified node
    * @param playout the specified game result
    */
  def backPropogation(node: Node, playout: Int): Unit = {
    var currentNode = node
    while (currentNode != null) {
      currentNode.nbVisit += 1
      if (currentNode.score != Int.MinValue) {
        if (currentNode.player == 2) {
          if (playout == 2) {
            currentNode.score += 10
          } else if (playout == 1) {
            currentNode.score -= 1
          } else {
            currentNode.score += 0.5
          }
        } else {
          if (playout == 1) {
            currentNode.score += 10
          } else if (playout == 2) {
            currentNode.score -= 1
          } else {
            currentNode.score += 0.5
          }
        }
      }
      currentNode = currentNode.parent
    }
  }

  /**
    * Display in console.err the specified node with the specified prefix.
    * @param node the specified node
    * @param prefix the specified prefix
    */
  private def logTree(node: Node, prefix: String = ""): Unit = {
    Console.err.println(prefix + "(" + node.board.toString + ") " + node.score + "/" + node.nbVisit)
    val newPrefix = prefix + " "
    node.subNodes.foreach(sn => logTree(sn, newPrefix))
  }
}

