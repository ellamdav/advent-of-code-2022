import scala.io.Source

object Day01 extends App {

  def part1(input: Seq[String]): Long = elfCalories(input).max

  def part2(input: Seq[String]): Long = elfCalories(input).sorted.takeRight(3).sum

  private def elfCalories(input: Seq[String]): Seq[Int] = lol(input).map(_.map(_.toInt).sum)

  private def lol(input: Seq[String]): Seq[Seq[String]] = input.foldLeft(Seq.empty[Seq[String]]) {
    case (acc, "")            => Nil +: acc
    case (head :: tail, next) => (next +: head) +: tail
    case (empty, next)        => empty :+ Seq(next)
  }

  val input = Source.fromResource("Day01.txt").getLines().toSeq

  println(part1(input))
  println(part2(input))

}
