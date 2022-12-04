import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day04Test extends AnyFlatSpec with Matchers {

  private val input =
    """2-4,6-8
      |2-3,4-5
      |5-7,7-9
      |2-8,3-7
      |6-6,4-6
      |2-6,4-8""".stripMargin.split("\n").toList

  "part1" should "return the number of assignment pairs where one is a superset of the other" in {
    Day04.part1(input) should be(2)
  }

  "part2" should "return the number of overlapping assignment pairs" in {
    Day04.part2(input) should be(4)
  }

}
