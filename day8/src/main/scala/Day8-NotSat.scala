package advent

import cats._
import cats.effect._
import fs2._
import cats.syntax.all._
import scala.collection.immutable.ArraySeq
import cats.data.NonEmptyList
import cats.parse.Accumulator

object Wires:
  type Pattern = Set[Char]
  case class Input(fromPatterns: Set[Pattern], toPatterns: NonEmptyList[Pattern])

object Day8NotSat extends AdventDay:
  import Wires.*
  import cats.parse.{Parser => P, Numbers => N}

  val pattern = P.charIn('a' to 'g').repAs[List[Char]].map(_.toSet)
  val line =
    (P.repSep(pattern, P.char(' ')) ~ (P.string(" | ") *> P.repSep(pattern, P.char(' ')))).map {
      case (pats, nums) => Input(pats.toList.toSet, nums)
    }
  override def puzzle1(in: Stream[IO, String]): IO[String] =
    in.map(line.parseAll)
      .flatMap(in => Stream.emits(in.toOption.get.toPatterns.toList))
      .filter { pat => pat.size == 2 || pat.size == 3 || pat.size == 4 || pat.size == 7 }
      .compile
      .count
      .map(c => s"$c patterns that encode 1, 4, 7 or 8")

  def decode(input: Input): Int =
    //it seems you should write a SAT solver, but you can just reason your way out
    val sized = input.fromPatterns.groupBy(_.size)
    val one = sized(2).head
    val four = sized(4).head
    val seven = sized(3).head
    val eight = sized(7).head

    val nine = sized(6).find(n => four.subsetOf(n)).get //nine is the one with 6 elements that ar eall in four
    val zero = sized(6).excl(nine).find(z => one.subsetOf(z)).get //zero is the the other one sized 6 that doesn't include one
    val six = sized(6).excl(nine).excl(zero).head //the last one sized 6 is 6

    val a = (seven -- one).head //segment a is the segment of 7 that's not in 1
    val d = (four -- zero).head //d is the one in four that's not in zero
    val b = (four -- (one + d)).head //b is the other one in 4 that's not in one
    val c = (one -- six).head //segment c is the segment in one that's not in six
    val f = (one - c).head //segment f is the other segment in one

    val three = sized(5).find(t => seven.subsetOf(t)).get // three is the one with 5 elements that's a subset of 7
    val two = (sized(5) - three).find(t => t.contains(c)).get // two is the one with c
    val five = (sized(5) - two - three).head // and the other one is five

    def decodeOutput(digit: Pattern) = digit match {
      case `one` => 1
      case `two` => 2
      case `three` => 3
      case `four` => 4
      case `five` => 5
      case `six` => 6
      case `seven` => 7
      case `eight` => 8
      case `nine` => 9
      case `zero` => 0 
    }

    input.toPatterns.toList match {
      case List(t, c, d, s) => 1000 * decodeOutput(t) + 100 * decodeOutput(c) + 10 * decodeOutput(d) + decodeOutput(s)
    }

  override def puzzle2(in: Stream[IO, String]): IO[String] =
    in.map(line.parseAll(_).toOption.get)
      .foldMap(decode)
      .compileSingleton
      .map(sum => s"patterns add up to $sum")
