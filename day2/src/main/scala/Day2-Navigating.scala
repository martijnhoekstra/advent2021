package advent

import fs2._
import cats._
import cats.effect._
import cats.syntax.all._

type Coords = (Int, Int)

enum Command:
  case ChangeAim(i: Int)
  case Move(n: Int)

case class Nav(pos: Coords, aim: Int)

object Day2Navigating extends IOApp.Simple:

  given Monoid[Coords] with
    def empty = (0, 0)
    def combine(x: Coords, y: Coords) = (x._1 + y._1, x._2 + y._2)

  override def run: IO[Unit] =
    val directions = "/input.txt".resourceLines[IO].takeWhile(!_.isBlank)
    for {
      destination1 <- directions.foldMap(parse1).compileSingleton
      destination2 <- directions
        .map(parse2)
        .fold(Nav(0 -> 0, 0)) {
          case (Nav((x, y), aim), Command.Move(n))      => Nav((x + n, y + (n * aim)), aim)
          case (Nav(coords, aim), Command.ChangeAim(n)) => Nav(coords, aim + n)
        }
        .compileSingleton
      _ <- IO(println(s"puzzle1: ${destination1._1 * destination1._2}"))
      _ <- IO(println(s"puzzle2: ${destination2.pos._1 * destination2.pos._2}"))
    } yield ()

  def parse1(line: String): Coords = line match {
    case s"forward $x" => (x.toInt, 0)
    case s"up $y"      => (0, -(y.toInt))
    case s"down $y"    => (0, y.toInt)
  }

  def parse2(line: String): Command = line match {
    case s"forward $x" => Command.Move(x.toInt)
    case s"up $y"      => Command.ChangeAim(-y.toInt)
    case s"down $y"    => Command.ChangeAim(y.toInt)
  }
