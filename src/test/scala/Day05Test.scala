import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day05Test extends AnyFlatSpec with Matchers {

  private val input =
    """    [D]    
      |[N] [C]    
      |[Z] [M] [P]
      | 1   2   3 
      |
      |move 1 from 2 to 1
      |move 3 from 1 to 3
      |move 2 from 2 to 1
      |move 1 from 1 to 2""".stripMargin.split("\n").toList

  "part1" should "move crates and report which crates are on top at the end" in {
    Day05.part1(input) should be("CMZ")
  }

  "part2" should "do the same, but using the CrateMover 9001" in {
    Day05.part2(input) should be("MCD")
  }

}
