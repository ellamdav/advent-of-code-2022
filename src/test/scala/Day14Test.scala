import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day14Test extends AnyFlatSpec with Matchers {

  private val input =
    """498,4 -> 498,6 -> 496,6
      |503,4 -> 502,4 -> 502,9 -> 494,9""".stripMargin.split("\n").toList

  "part1" should "count the number of units of sand at rest" in {
    Day14.part1(input) should be(24)
  }

  "part2" should "do the same, assuming a floor at maxy + 2" in {
    Day14.part2(input) should be(93)
  }

}
