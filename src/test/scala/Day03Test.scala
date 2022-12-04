import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day03Test extends AnyFlatSpec with Matchers {

  private val input =
    """vJrwpWtwJgWrhcsFMMfFFhFp
      |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
      |PmmdzqPrVvPwwTWBwg
      |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
      |ttgJtRGJQctTZtZT
      |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin.split("\n").toList

  "part1" should "sum the priorities of the misfiled items" in {
    Day03.part1(input) should be(157)
  }

  "part2" should "sum the priorities of the badges for each group" in {
    Day03.part2(input) should be(70)
  }

}
