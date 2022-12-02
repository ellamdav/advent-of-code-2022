import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day02Test extends AnyFlatSpec with Matchers {

  private val input =
    """A Y
      |B X
      |C Z""".stripMargin.split("\n").toList

  "part1" should "sum all your scores" in {
    Day02.part1(input) should be(15)
  }

  "part2" should "calculate what to play to meet the strategy" in {
    Day02.part2(input) should be(12)
  }

}
