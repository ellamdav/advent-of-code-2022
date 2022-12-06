import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day06Test extends AnyFlatSpec with Matchers {

  private val input = """"""

  "part1" should "find the position of the end of the start-of-packet marker" in {
    Day06.part1("mjqjpqmgbljsphdztnvjfqwrcgsmlb")    should be(7)
    Day06.part1("bvwbjplbgvbhsrlpgdmjqwftvncz")      should be(5)
    Day06.part1("nppdvjthqldpwncqszvftbrmjlhg")      should be(6)
    Day06.part1("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") should be(10)
    Day06.part1("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")  should be(11)
  }

  "part2" should "find the position of the end of the start-of-message marker" in {
    Day06.part2("mjqjpqmgbljsphdztnvjfqwrcgsmlb")    should be(19)
    Day06.part2("bvwbjplbgvbhsrlpgdmjqwftvncz")      should be(23)
    Day06.part2("nppdvjthqldpwncqszvftbrmjlhg")      should be(23)
    Day06.part2("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") should be(29)
    Day06.part2("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")  should be(26)
  }

}
