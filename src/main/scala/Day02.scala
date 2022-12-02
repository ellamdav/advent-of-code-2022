import scala.io.Source

object Day02 extends App {

  lazy val ROCK     = 1
  lazy val PAPER    = 2
  lazy val SCISSORS = 3

  lazy val WIN  = 6
  lazy val LOSE = 0
  lazy val DRAW = 3

  def part1(input: Seq[String]): Long =
    input
      .map(_.split(" ").toSeq.map {
        case "A" | "X" => ROCK
        case "B" | "Y" => PAPER
        case "C" | "Z" => SCISSORS
      })
      .map {
        case Seq(ROCK, PAPER)     => PAPER + WIN
        case Seq(ROCK, SCISSORS)  => SCISSORS + LOSE
        case Seq(PAPER, ROCK)     => ROCK + LOSE
        case Seq(PAPER, SCISSORS) => SCISSORS + WIN
        case Seq(SCISSORS, ROCK)  => ROCK + WIN
        case Seq(SCISSORS, PAPER) => PAPER + LOSE
        case Seq(_, mine)         => mine + DRAW
      }
      .sum

  def part2(input: Seq[String]): Long =
    input
      .map(_.split(" ").toSeq.map {
        case "A" => ROCK
        case "B" => PAPER
        case "C" => SCISSORS
        case "X" => LOSE
        case "Y" => DRAW
        case "Z" => WIN
      })
      .map {
        case Seq(ROCK, LOSE)     => SCISSORS + LOSE
        case Seq(ROCK, WIN)      => PAPER + WIN
        case Seq(PAPER, LOSE)    => ROCK + LOSE
        case Seq(PAPER, WIN)     => SCISSORS + WIN
        case Seq(SCISSORS, LOSE) => PAPER + LOSE
        case Seq(SCISSORS, WIN)  => ROCK + WIN
        case Seq(theirs, DRAW)   => theirs + DRAW
      }
      .sum

  val input = Source.fromResource("Day02.txt").getLines().toSeq

  println(part1(input))
  println(part2(input))

}
