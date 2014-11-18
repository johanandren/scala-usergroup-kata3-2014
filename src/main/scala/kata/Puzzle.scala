import scala.collection.Set

object Puzzle {

  def solve: List[List[Int]] = {
    val indices =
      List(
        // horizontal
        List(0, 1, 2), List(3, 4, 5), List(6, 7, 8),
        // vertical
        List(0, 3, 6), List(1, 4, 7), List(2, 5, 8),
        // diagonal
        List(0, 4, 8), List(2, 4, 6)
      )

    def valid(items: Seq[Int]) = indices.map(_ map items).map(_.sum).toSet.size == 1

    List(1,2,3,4,5,6,7,8,9)
      .permutations
      .collectFirst { case l if valid(l) => l.grouped(3).toList.map(_.toList) }
      .getOrElse(throw new RuntimeException("No solution found"))
  }
}