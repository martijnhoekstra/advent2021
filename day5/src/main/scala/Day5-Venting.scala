package advent

import cats._
import cats.effect._
import cats.syntax.all._

import scala.collection.immutable.BitSet

object Day5Venting extends IOApp.Simple:
  import Coords._
  import cats.parse.{Parser => P, Numbers => N}

  def combinations[A](l: LazyList[A]): LazyList[(A, A)] = l match {
    case head #:: tail => tail.map(head -> _) #::: combinations(tail)
    case _             => LazyList.empty
  }

  val pLine: P[Line] =
    val int = N.digits.map(_.toInt)
    val pcoord = (int ~ (P.char(',') *> int)).map(Coord.apply)
    (pcoord ~ (P.string(" -> ") *> pcoord)).map{ case (c1, c2) => Line(c1, c2)}

  override def run: IO[Unit] = {
    for {
      input <- "/input.txt"
        .resourceLines[IO]
        .map(l => pLine.parseAll(l).toOption.get)
        .compile
        .to(LazyList)
      multiple = combinations(input).foldMap(intersections) // it's still always foldMap
      _ <- IO(println(multiple.size))
    } yield ()
  }
