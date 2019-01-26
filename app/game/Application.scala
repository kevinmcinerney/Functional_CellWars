package game

/**
  * Created by kevin on 26/01/19.
  */
object Application extends App{

  val myGame = new Game(20)

  val numPerTeam = myGame.board.dimensions / 4

  val teamOne = myGame.loadTeam(0, numPerTeam)

  val teamTwo = myGame.loadTeam(myGame.board.dimensions - 4, numPerTeam)

  println("Board " + myGame.board.dimensions)

  println("TeamOne " + teamOne)

  println("TeamTwo " + teamTwo)

}
