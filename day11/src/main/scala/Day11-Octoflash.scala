package advent

import cats._
import cats.effect._
import fs2._
import cats.syntax.all._
import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag
import scala.collection.immutable.BitSet

object Grids:
  val width = 10
  val height = 10
  opaque type Grid[A] = ArraySeq[A]
  extension [A](g: Grid[A])
    def apply(c: Coord) = g(c)
    def neighbourhoods: Seq[(A, List[A])] = gridCoords.map(c => g(c) -> c.neighbours.map(g.apply))
    def updated(c: Coord, a: A): Grid[A] = g.updated(c, a)

  object Grid:
    def apply[A: ClassTag](in: Seq[A]): Grid[A] = ArraySeq.from(in)

  opaque type Coord = Int
  object Coord:
    def apply(x: Int, y: Int): Coord = (x * width) + y
  extension (c: Coord)
    def x: Int = c / width
    def y: Int = c % width
    def neighbours: List[Coord] = for
      dx <- List(-1, 0, 1)
      dy <- List(-1, 0, 1)
      (x, y) = (c.x + dx, c.y + dy) if x >= 0 && x < width && y >= 0 && y < height
    yield Coord(x, y)

  val gridCoords: List[Coord] = (for
    x <- 0 until width
    y <- 0 until height
  yield Coord(x, y)).toList

  def load[A: ClassTag](data: Seq[Seq[A]]): Grid[A] = Grid(
    data.iterator.flatMap(_.iterator).to(ArraySeq)
  )
  def coordSet: Set[Coord] = BitSet.empty

object Day11Octoflash extends AdventDay:
  import Grids.*

  def step(grid: Grid[Int]) =
    def rec(blinked: Set[Coord], grid: Grid[Int], toIncrease: List[Coord]): (Int, Grid[Int]) =
      toIncrease match {
        case Nil => (blinked.size, grid)
        case head :: tail => {
          val old = grid(head)
          if blinked.contains(head) then rec(blinked, grid, tail)
          else if old == 9 then
            rec(blinked.incl(head), grid.updated(head, 0), head.neighbours ::: tail)
          else rec(blinked, grid.updated(head, old + 1), tail)
        }
      }

    rec(coordSet, grid, gridCoords)

  override def puzzle1(in: Stream[IO, String]): IO[String] =
    for
      input <- in
        .map(line => line.map(_.toString.toInt))
        .compile
        .toList
    yield {
      val grid = Grids.load(input)
      val steps = Stream.unfold(grid)(next => Some(step(next)))
      val total = steps.take(100).foldMap(identity).compile.last.get
      s"$total blinked octopodes"
    }

  override def puzzle2(in: Stream[IO, String]): IO[String] =
    for
      input <- in
        .map(line => line.map(_.toString.toInt))
        .compile
        .toList
    yield {
      val grid = Grids.load(input)
      val steps = Stream.unfold(grid)(next => Some(step(next)))
      val sync = steps.zipWithIndex.collectFirst { case (100, index) => index }.compile.last.get
      s"synchronized after ${sync + 1} steps"
    }
