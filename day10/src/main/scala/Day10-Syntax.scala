package advent

import cats._
import cats.effect._
import fs2._
import cats.syntax.all._
import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

object Day10Syntax extends AdventDay:

  def checkLine(line: String) =
    def rec(stack: List[Char], index: Int): Either[Char, List[Char]] =
      if index >= line.length then stack.asRight
      else
        val ch = line(index)
        (ch, stack) match {
          case ('(', _)       => rec(')' :: stack, index + 1)
          case ('{', _)       => rec('}' :: stack, index + 1)
          case ('<', _)       => rec('>' :: stack, index + 1)
          case ('[', _)       => rec(']' :: stack, index + 1)
          case (_, `ch` :: t) => rec(t, index + 1)
          case _              => ch.asLeft
        }
    rec(Nil, 0)

  def score(ch: Char) = ch match {
    case ')' => 3 // 1 * 3
    case ']' => 57 // 19 * 3
    case '}' => 1197 // 299 * 3
    case '>' => 25137 // 8379 * 3  so I guess we have that going for us, which is nice
  }

  def score(stack: List[Char]) =
    stack.foldLeft(0L)((agg, ch) =>
      (5 * agg) + (ch match {
        case ')' => 1
        case ']' => 2
        case '}' => 3
        case '>' => 4
      })
    )

  def median[A: Ordering](as: IndexedSeq[A]) =
    as.sorted.apply(as.length / 2)

  override def puzzle1(in: Stream[IO, String]): IO[String] =
    in.map(checkLine)
      .collect { case Left(ch) => score(ch) }
      .foldMonoid
      .map(_.toString)
      .compileSingleton

  override def puzzle2(in: Stream[IO, String]): IO[String] =
    in.map(checkLine)
      .collect { case Right(stack) => score(stack) }
      .compile
      .toVector
      .map(median)
      .map(_.toString)
