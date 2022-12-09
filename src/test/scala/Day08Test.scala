import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day08Test extends AnyFlatSpec with Matchers {

  private val input =
    """30373
      |25512
      |65332
      |33549
      |35390""".stripMargin.split("\n").toList

  "part1" should "count the trees that are visible from outside the grid" in {
    Day08.part1(input) should be(21)
  }

  "part2" should "???" in {
    pending
    Day08.part2(input) should be(0)
  }

}
