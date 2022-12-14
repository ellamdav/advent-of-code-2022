import scala.annotation.tailrec
import scala.io.Source

object Day14 extends App {

  private def parse(input: Seq[String]): Map[(Int, Int), Char] =
    input.map(seam).fold(Map.empty)(_ ++ _)

  private def seam(coordinates: String): Map[(Int, Int), Char] =
    coordinates
      .split(" -> ")
      .toList
      .map(_.split(",").toList.map(_.toInt))
      .sliding(2)
      .map(line(_))
      .fold(Map.empty)(_ ++ _)

  @tailrec
  private def line(points: List[List[Int]], acc: Map[(Int, Int), Char] = Map.empty): Map[(Int, Int), Char] =
    points match {
      case start :: end :: Nil if start == end => addToSeam(acc, start)
      case start :: end :: Nil                 =>
        (end.head - start.head).sign match {
          case 1  => line(List(List(start.head + 1, start.last), end), addToSeam(acc, start))
          case -1 => line(List(List(start.head - 1, start.last), end), addToSeam(acc, start))
          case 0  =>
            (end.last - start.last).sign match {
              case 1 => line(List(List(start.head, start.last + 1), end), addToSeam(acc, start))
              case _ => line(List(List(start.head, start.last - 1), end), addToSeam(acc, start))
            }
        }
    }

  private def addToSeam(acc: Map[(Int, Int), Char], position: List[Int]) =
    acc + ((position.head, position.last) -> '#')

  def part1(input: Seq[String]): Long = {
    val grid     = parse(input)
    printMap(grid)
    val overflow = grid.keys.map(_._2).max + 1
    flow(grid, overflow)
  }

  def printMap(grid: Map[(Int, Int), Char]): Unit = {
    val minx = grid.keys.map(_._1).min
    val maxx = grid.keys.map(_._1).max
    val miny = 0
    val maxy = grid.keys.map(_._2).max
    miny to maxy foreach { y =>
      minx to maxx foreach { x =>
        print(grid.getOrElse((x, y), '.'))
      }
      println
    }
    println("-------------")
  }

  lazy val flowDirections = List(0, -1, 1)

  @tailrec
  def flow(grid: Map[(Int, Int), Char], overflow: Int, units: Int = 0, sand: (Int, Int) = (500, 0)): Int =
    sand match {
      case (_, `overflow`) =>
        printMap(grid)
        units
      case _               =>
        flowDirections.find { d =>
          val nextPosition = (sand._1 + d, sand._2 + 1)
          grid.get(nextPosition).isEmpty
        } match {
          case None                    =>
            val newGrid = grid + (sand -> 'o')
            if (sand._2 == 0) {
              printMap(newGrid)
              units + 1
            } else {
              flow(newGrid, overflow, units + 1)
            }
          case Some(flowableDirection) => flow(grid, overflow, units, (sand._1 + flowableDirection, sand._2 + 1))
        }
    }

  def part2(input: Seq[String]): Long = {
    val grid          = parse(input)
    val minx          = grid.keys.map(_._1).min
    val maxx          = grid.keys.map(_._1).max
    val maxy          = grid.keys.map(_._2).max
    val buffer        = 1000
    val gridWithFloor = grid ++ line(List(List(minx - buffer, maxy + 2), List(maxx + buffer, maxy + 2)))
    printMap(gridWithFloor)
    flow(gridWithFloor, Int.MaxValue)
  }

  val input = Source.fromResource("Day14.txt").getLines().toSeq

  println(part1(input))
  println(part2(input))

}
