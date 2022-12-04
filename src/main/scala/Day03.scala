import scala.io.Source

object Day03 extends App {

  def part1(rucksacks: Seq[String]): Long =
    rucksacks
      .map { r =>
        r.toList.splitAt(r.length / 2)
      }
      .map { compartments =>
        compartments._1.find(c => compartments._2.contains(c))
      }
      .collect {
        case Some(character) if character.isUpper => character.toInt - 38
        case Some(character)                      => character.toInt - 96
      }
      .sum

  def part2(rucksacks: Seq[String]): Long =
    rucksacks
      .grouped(3)
      .map { groupRucksacks =>
        groupRucksacks.head.toList.find { c =>
          groupRucksacks(1).contains(c) && groupRucksacks.last.contains(c)
        } match {
          case Some(character) if character.isUpper => character.toInt - 38
          case Some(character)                      => character.toInt - 96
        }

      }
      .sum

  val input = Source.fromResource("Day03.txt").getLines().toSeq

  println(part1(input))
  println(part2(input))

}
