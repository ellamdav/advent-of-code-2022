import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

case class Instruction(count: Int, from: Int, to: Int)

object Day05 extends App {

  implicit class RichStack[T](stack: mutable.Stack[T]) {
    def popN(n: Int): Seq[T] =
      (1 to n map { _ =>
        stack.pop()
      }).reverse
  }

  def part1(input: Seq[String]): String = {
    val (cargoBay, instructions) = parse(input)
    moveOneCrateAtATime(cargoBay, instructions)
  }

  def part2(input: Seq[String]): String = {
    val (cargoBay, instructions) = parse(input)
    moveMultipleCrates(cargoBay, instructions)
  }

  private def parse(input: Seq[String]): (Map[Int, mutable.Stack[String]], Seq[Instruction]) = {
    val (stacks, rawInstructions) = input.splitAt(input.indexOf(""))
    val cargoBay                  = stacks.reverse.map { row =>
      row.zipWithIndex
        .filter {
          case (_, i) if i % 4 == 1 => true
          case _ => false
        }
        .map(_._1)
    } match {
      case head :: tail =>
        head.map { pos =>
          val stackContents: Seq[String] = tail.flatMap { row =>
            row.zipWithIndex
              .filter {
                case (_, i) if i == pos.toString.toInt - 1 => true
                case _                                     => false
              }
              .map(_._1.toString)
              .filter(_.trim.nonEmpty)
          }.reverse
          val stack                      = mutable.Stack[String](stackContents: _*)
          (pos.toString.toInt, stack)
        }.toMap
    }

    val instruction = "move (\\d+) from (\\d+) to (\\d+)".r

    val instructions = rawInstructions.flatMap {
      case instruction(count, from, to) => Some(Instruction(count.toInt, from.toInt, to.toInt))
      case _                            => None
    }

    (cargoBay, instructions)
  }

  @tailrec
  private def moveOneCrateAtATime(cargoBay: Map[Int, mutable.Stack[String]], instructions: Seq[Instruction]): String =
    instructions match {
      case Nil                                               => getTopOfStacks(cargoBay)
      case Instruction(count, from, to) :: tail if count > 0 =>
        cargoBay(to).push(cargoBay(from).pop())
        moveOneCrateAtATime(cargoBay, Instruction(count - 1, from, to) +: tail)
      case _ :: tail                                         => moveOneCrateAtATime(cargoBay, tail)
    }

  private def getTopOfStacks(cargoBay: Map[Int, mutable.Stack[String]]) =
    (cargoBay.keys.toList.sorted.map(cargoBay(_).pop())).mkString

  @tailrec
  private def moveMultipleCrates(cargoBay: Map[Int, mutable.Stack[String]], instructions: Seq[Instruction]): String =
    instructions match {
      case Nil                                               => getTopOfStacks(cargoBay)
      case Instruction(count, from, to) :: tail if count > 0 =>
        cargoBay(to).pushAll(cargoBay(from).popN(count))
        moveMultipleCrates(cargoBay, tail)
    }

  val input = Source.fromResource("Day05.txt").getLines().toSeq

  println(part1(input))
  println(part2(input))

}
