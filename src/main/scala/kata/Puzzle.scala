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

    def loop(left: Set[Int], matrix: List[Int]): Option[List[Int]] =
      if (left.isEmpty) {
        if (valid(matrix)) Some(matrix)
        else None
      } else {
        left.view
          .map(n => loop(left - n, n :: matrix))
          .collectFirst { case Some(sol) => sol }
      }

    def valid(items: List[Int]) = indices.map(_ map items).map(_.sum).toSet.size == 1

    loop(Set(1,2,3,4,5,6,7,8,9), List[Int]())
      .map(_.grouped(3).toList)
      .getOrElse(throw new RuntimeException("No solution found"))
  }
}