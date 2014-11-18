import scala.util.Random

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

    val solution = Array.tabulate(9)(n => n + 1)
    def valid = indices.map(_.map(solution(_))).map(_.sum).toSet.size == 1
    def rng() = Random.nextInt(9)
    while (!valid) {
      // (LOL)
      val idx1 = rng()
      val idx2 = rng()
      val a = solution(idx1)
      solution(idx1) = solution(idx2)
      solution(idx2) = a
    }

    solution.toList.grouped(3).toList
  }
}