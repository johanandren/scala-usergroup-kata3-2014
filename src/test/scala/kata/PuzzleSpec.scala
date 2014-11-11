import org.specs2.mutable.Specification

class PuzzleSpec extends Specification {

  lazy val result = Puzzle.solve

  "The puzzle solution" should {

    "be a 3x3 matrix" in {
      result must haveSize(3)
      result must forall (haveSize(3))
    }

    "only contain the number 1 to 9" in {
      result.flatten must forall (between (1, 9))
    }

    "only have each number once" in {
      result.flatten.toSet must haveSize (9)
    }

    "have the same sum across horizontally, vertically and diagonally" in {
      val indices =
        List(
          // horizontal
          List(0, 1, 2), List(3, 4, 5), List(6, 7, 8),
          // vertical
          List(0, 3, 6), List(1, 4, 7), List(2, 5, 9),
          // diagonal
          List(0, 4, 9), List(2, 4, 6)
        )

      val flatResult = result.flatten
      val allSums = indices map (_ map (flatResult(_)))

      allSums.toSet must haveSize (1)
    }

  }

}