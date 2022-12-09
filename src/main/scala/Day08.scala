import scala.collection.mutable
import scala.io.Source

object Day08 extends App {

  def part1(input: Seq[String]): Long = {
    val visible: mutable.Map[(Int, Int), Boolean] = mutable.Map.empty
    val maxy                                      = input.length - 1
    val maxx                                      = input.head.length - 1

    def horizontal(input: Seq[String], rowFn: String => Seq[(Char, Int)], visible: mutable.Map[(Int, Int), Boolean]) =
      input.zipWithIndex.foreach { case (row, y) =>
        rowFn(row).foldLeft(0) { case (max, (h, x)) =>
          val height = h.toString.toInt
          if (height > max || y == 0 || y == maxy) {
            visible((x, y)) = true
            height
          } else {
            max
          }
        }
      }

    def vertical(input: Seq[String], rowFn: String => Seq[(Char, Int)], visible: mutable.Map[(Int, Int), Boolean]) =
      input.zipWithIndex.foreach { case (row, x) =>
        rowFn(row).foldLeft(0) { case (max, (h, y)) =>
          val height = h.toString.toInt
          if (height > max || x == 0 || x == maxx) {
            visible((x, y)) = true
            height
          } else {
            max
          }
        }
      }

    horizontal(input, _.zipWithIndex, visible)
    horizontal(input, _.zipWithIndex.reverse, visible)
    vertical(input.transpose.map(_.mkString), _.zipWithIndex, visible)
    vertical(input.transpose.map(_.mkString), _.zipWithIndex.reverse, visible)

    visible.size
  }

  def part2(input: Seq[String]): Long = 0

  val input = Source.fromResource("Day08.txt").getLines().toSeq

  println(part1(input))
  println(part2(input))

}
