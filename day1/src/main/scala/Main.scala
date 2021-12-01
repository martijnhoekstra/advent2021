import scala.io.Source
import scala.io.BufferedSource

@main def day1: Unit =
  def source = Source.fromResource("input.txt").getLines.map(_.toInt)
  var answer1 = puzzle1(source)
  val answer2 = puzzle2(source)
  println(s"first: $answer1")
  println(s"second: $answer2")

def puzzle1(input: => Iterator[Int]) =
  input
    .sliding(2)
    .count {
      case Seq(first, second) => first < second
      case _                  => false
    }

def puzzle2(input: => Iterator[Int]) =
  val triplets = input.sliding(3).collect { case Seq(x, y, z) => x + y + z }
  puzzle1(triplets.iterator)
