package advent

import cats._
import cats.effect._
import fs2._
import cats.syntax.all._
import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

object Grids:
  val width = 100
  val height = 100
  opaque type Grid[A] = ArraySeq[A]
  extension [A](g: Grid[A])
    def apply(c: Coord) = g(c)
    def coords = for
      y <- 0 until height
      x <- 0 until width
    yield Coord(x, y)
    def neighbourhoods: Seq[(A, List[A])] = g.coords.map(c => g(c) -> c.neighbours.map(g.apply))

  object Grid:
    def apply[A: ClassTag](in: Seq[A]): Grid[A] = ArraySeq.from(in)

  opaque type Coord = Int
  object Coord:
    def apply(x: Int, y: Int): Coord = (x * width) + y
  extension (c: Coord)
    def x: Int = c / width
    def y: Int = c % width
    def neighbours: List[Coord] = List(
      Option.when(x != 0)(Coord(x - 1, y)),
      Option.when(x < (width - 1))(Coord(x + 1, y)),
      Option.when(y > 0)(Coord(x, y - 1)),
      Option.when(y < (height - 1))(Coord(x, y + 1))
    )
      .collect { case Some(x) => x }

  def load[A: ClassTag](data: Seq[Seq[A]]): Grid[A] = Grid(
    data.iterator.flatMap(_.iterator).to(ArraySeq)
  )

object Day9Lava extends AdventDay:

  override def puzzle1(in: Stream[IO, String]): IO[String] =
    val linesio = in.map(line => line.map(ch => ch.toString.toInt)).compile.toVector
    for
      lines <- linesio
    yield {
      import Grids._
      val grid: Grid[Int] = Grids.load(lines)
      val depth = grid.neighbourhoods.collect {
        case (it, neighbours) if neighbours.forall(_ > it) => it
      }.toList
      val danger = depth.foldMap(_ + 1)
      danger.toString
    }

  override def puzzle2(in: Stream[IO, String]): IO[String] = IO("unsolved")
