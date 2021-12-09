package advent

import cats.effect._
import fs2._

trait AdventDay extends IOApp.Simple:

  def puzzle1(input: Stream[IO, String]): IO[String]
  def puzzle2(lines: Stream[IO, String]): IO[String]

  override def run: IO[Unit] =
    val in = "/input.txt".resourceLines[IO]
    for {
      p1 <- puzzle1(in)
      p2 <- puzzle2(in)
      _ <- IO(println(s"Puzzle 1: $p1"))
      _ <- IO(println(s"Puzzle 2: $p2"))
    } yield ()
