import org.scalatest._
import flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day01Test extends AnyFlatSpec with Matchers {

  private val input =
    """1000
      |2000
      |3000
      |
      |4000
      |
      |5000
      |6000
      |
      |7000
      |8000
      |9000
      |
      |10000""".stripMargin.split("\n").toList

  "part1" should "find the highest calorie count of all the elves" in {
    Day01.part1(input) should be(24000)
  }

  "part2" should "find the total calorie count of the top three elves" in {
    Day01.part2(input) should be(45000)
  }

}
