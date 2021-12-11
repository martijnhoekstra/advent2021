package advent

import fs2._
import cats.effect._
import cats.syntax.all._

object Day1fs2 extends IOApp.Simple:

  override def run: IO[Unit] =
    val source = "/input.txt".resourceLines[IO].takeWhile(!_.isBlank).map(_.toInt)
    for
     r1 <- puzzle1(source)
     r2 <- puzzle2(source)
     _ <- IO(println(s"puzzle1: $r1"))
     _ <- IO(println(s"puzzle2: $r2"))
    yield ()

def puzzle1(input: StreamIO[Int]) =
  input
    .sliding(2)
    .foldMap(ch => if ch.size == 2 && ch(0) < ch(1) then 1 else 0)
    .compileSingleton

def puzzle2(input: StreamIO[Int]) =
  val triplets = input.sliding(3).collect { case ch if ch.size == 3 => ch.fold }
  puzzle1(triplets)
