import scala.io.Source

object Day06 extends App {

  def part1(input: String): Long = findMarker(input, 4)

  def part2(input: String): Long = findMarker(input, 14)

  private def findMarker(input: String, markerLength: Int): Long = {
    val marker = input
      .sliding(markerLength)
      .find { sequence =>
        val sortedSequence = sequence.sorted
        sortedSequence.replaceFirst("(.)\\1+", "") == sortedSequence
      }
      .get
    input.indexOf(marker) + markerLength
  }

  val input = Source.fromResource("Day06.txt").getLines().toSeq.head

  println(part1(input))
  println(part2(input))

}
