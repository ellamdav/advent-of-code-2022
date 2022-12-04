import scala.io.Source

object Day04 extends App {

  def part1(elfPairs: Seq[String]): Long =
    countWhere(elfPairs, (f, s) => f.subsetOf(s) || s.subsetOf(f))

  def part2(elfPairs: Seq[String]): Long =
    countWhere(elfPairs, (f, s) => f.intersect(s).nonEmpty)

  private def countWhere(elfPairs: Seq[String], setFn: (Set[Int], Set[Int]) => Boolean) =
    elfPairs
      .map(p => p.split(",").toList)
      .map { case first :: second :: Nil =>
        val firstSet  = rangeToSet(first)
        val secondSet = rangeToSet(second)
        setFn(firstSet, secondSet)
      }
      .count(identity)

  private def rangeToSet(range: String) =
    range.split("-").toList match {
      case start :: end :: Nil => (start.toInt to end.toInt).toSet
    }

  val input = Source.fromResource("Day04.txt").getLines().toSeq

  println(part1(input))
  println(part2(input))

}
