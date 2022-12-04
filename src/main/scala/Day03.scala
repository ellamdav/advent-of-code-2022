import scala.io.Source

object Day03 extends App {

  def part1(rucksacks: Seq[String]): Long =
    rucksacks
      .map(rucksack => rucksack.toList.splitAt(rucksack.length / 2))
      .map(compartments => compartments._1.find(c => compartments._2.contains(c)))
      .collect(priorityOfMaybeItem)
      .sum

  def part2(rucksacks: Seq[String]): Long =
    rucksacks
      .grouped(3)
      .collect { case first :: second :: third :: Nil =>
        priorityOfMaybeItem(first.toList.find(c => second.contains(c) && third.contains(c)))
      }
      .sum

  private lazy val priorityOfMaybeItem: PartialFunction[Option[Char], Int] = {
    case Some(character) if character.isUpper => character.toInt - 38
    case Some(character)                      => character.toInt - 96
  }

  val input = Source.fromResource("Day03.txt").getLines().toSeq

  println(part1(input))
  println(part2(input))

}
