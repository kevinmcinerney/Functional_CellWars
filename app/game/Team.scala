package game

/**
  * Created by kevin on 30/04/17.
  */
case class Team(cells: List[Cell]) {

  def size = cells.size

  def mergedNeeded(team: Team): Boolean = {
    team.allPairs(2)
      .exists(pair => pair.head contains pair.last)
  }

  def recMerge(team: Team): Team = {
    if (mergedNeeded(team)) recMerge(new Team(this.merge(team)))
    else team
  }

  def mergeCellPairs(cellPairs: List[List[Cell]]): List[Cell] = {
    cellPairs.map(pair => pair.head merge pair.last get)
  }

  def merge(team: Team): List[Cell] = {
    val (merged, unmerged) = team
      .allPairs(2)
      .partition(pair => pair.head contains pair.last)

    val mergedCells = mergeCellPairs(merged)

    val unmergedCells = unmerged
      .flatten
      .filterNot(merged.flatten.contains(_))
      .distinct

    mergedCells ::: unmergedCells
  }

  def allPairs(n: Int): List[List[Cell]] = this.cells.combinations(n).toList

  // TEST
  def up(team: Team, point: Coordinate): Team = {
    val (mover, others) = team.cells.partition(_.contains(point))
    Team(others :+ mover.head.up)
  }

  // TEST
  def right(team: Team, point: Coordinate): Team = {
    val (mover, others) = team.cells.partition(_.contains(point))
    Team(others :+ mover.head.right)
  }

  // TEST
  def down(team: Team, point: Coordinate): Team = {
    val (mover, others) = team.cells.partition(_.contains(point))
    Team(others :+ mover.head.down)
  }

  // TEST
  def left(team: Team, point: Coordinate): Team = {
    val (mover, others) = team.cells.partition(_.contains(point))
    Team(others :+ mover.head.left)
  }
}
