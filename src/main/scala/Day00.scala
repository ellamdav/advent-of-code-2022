import scala.io.Source

object Day00 extends App {

  def part1(input: Seq[String]): Long = 0

  def part2(input: Seq[String]): Long = 0

  val input = Source.fromResource("Day00.txt").getLines().toSeq

  println(part1(input))
  println(part2(input))

}
