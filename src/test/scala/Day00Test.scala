import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day00Test extends AnyFlatSpec with Matchers {

  private val input = """""".stripMargin.split("\n").toList

  "part1" should "???" in {
    Day00.part1(input) should be(0)
  }

  "part2" should "???" in {
    pending
    Day00.part2(input) should be(0)
  }

}
