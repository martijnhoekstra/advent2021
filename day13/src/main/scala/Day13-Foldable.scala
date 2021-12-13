package advent

import cats._
import cats.effect._
import fs2._
import cats.syntax.all._
import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag
import scala.collection.immutable.BitSet
import cats.parse.{Parser => P, Numbers => N}

case class Coord(x: Int, y: Int)
sealed trait FoldInstruction
case class FoldAlongX(x: Int) extends FoldInstruction
case class FoldAlongY(y: Int) extends FoldInstruction

object Day13Foldable extends AdventDay:

  val pInt = N.digits.map(_.toInt)
  val pCoord = (pInt ~ (P.char(',') *> pInt)).map(Coord(_, _))
  val pFoldInstruction = {
    val parts = P.string("fold along ") *>
      (P.char('x').as(FoldAlongX(_)) | P.char('y').as(FoldAlongY(_))) ~
      (P.char('=') *> pInt)
    parts.map((f, a) => f(a))
  }
  val instructionOrCoord = P.eitherOr(pCoord, pFoldInstruction)

  def fold(sheet: Set[Coord], instruction: FoldInstruction): Set[Coord] = instruction match
    case FoldAlongX(xf) =>
      sheet.collect {
        case Coord(x, y) if x > xf           => Coord(xf - (x - xf), y)
        case unmoved @ Coord(x, _) if x < xf => unmoved
      }
    case FoldAlongY(yf) =>
      sheet.collect {
        case Coord(x, y) if y > yf           => Coord(x, (yf - (y - yf)))
        case unmoved @ Coord(_, y) if y < yf => unmoved
      }

  override def puzzle1(in: Stream[IO, String]): IO[String] =
    val steps = in
      .filterNot(_.isEmpty)
      .scan[Set[Coord]](Set.empty[Coord])((set, line) => {
        instructionOrCoord.parseAll(line) match
          case Right(Left(foldInstruction)) => fold(set, foldInstruction)
          case Right(Right(coord))          => set + coord
          case Left(parseError)             => throw new Exception(s"parse error $parseError")
      })
    // get the first step where the set is smaller than before: that's after the first time we folded
    val firstFold = steps.sliding(2).collectFirst {
      case chunk if (chunk(0).size > chunk(1).size) => chunk(1)
    }

    firstFold.map(set => s"after step 1, we have ${set.size} dots").compileSingleton

  def printSheet(coords: Set[Coord]) =
    if coords.isEmpty then ""
    else {
      val minx = coords.minBy(_.x).x
      val maxx = coords.maxBy(_.x).x
      val miny = coords.minBy(_.y).y
      val maxy = coords.maxBy(_.y).y
      (miny to maxy)
        .map(y => {
          (minx to maxx).map(x => if coords.contains(Coord(x, y)) then 'X' else '.').mkString
        })
        .mkString("\n", "\n", "\n")
    }

  override def puzzle2(in: Stream[IO, String]): IO[String] =
    val steps = in
      .filterNot(_.isEmpty)
      .scan[Set[Coord]](Set.empty[Coord])((set, line) => {
        instructionOrCoord.parseAll(line) match {
          case Right(Left(foldInstruction)) => fold(set, foldInstruction)
          case Right(Right(coord))          => set + coord
          case Left(parseError)             => throw new Exception(s"parse error $parseError")
        }
      })

    steps.lastOr(Set.empty).map(printSheet).compileSingleton
