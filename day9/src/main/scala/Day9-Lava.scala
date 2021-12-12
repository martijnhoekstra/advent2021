package advent

import cats._
import cats.effect._
import fs2._
import cats.syntax.all._
import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag
import scala.collection.immutable.BitSet
import simulacrum.op

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

  def coordSet: Set[Coord] = BitSet.empty

object Day9Lava extends AdventDay:
  import Grids.*

  override def puzzle1(in: Stream[IO, String]): IO[String] =
    val linesio = in.map(line => line.map(ch => ch.toString.toInt)).compile.toVector
    for lines <- linesio
    yield
      val grid: Grid[Int] = Grids.load(lines)
      val depth = grid.neighbourhoods.collect {
        case (it, neighbours) if neighbours.forall(_ > it) => it
      }.toList
      val danger = depth.foldMap(_ + 1)
      danger.toString

  def reservoirs(grid: Grid[Int]): List[Set[Coord]] =
    // I sincerely hope nobody, including future me, will ever have to try to make sense of what I wrote here.
    def rec(
        fullReservoirs: List[Set[Coord]],
        closedRidge: Set[Coord],
        openRidge: Set[Coord],
        current: Set[Coord],
        open: Set[Coord]
    ): List[Set[Coord]] =
      println(s"closed reservoirs: ${fullReservoirs.flatten.length}, closedRidge: ${closedRidge.size}, openRidge: ${openRidge.size}, current: ${current.size}, open: ${open.size}")
      def seen(coord: Coord) =
        closedRidge.contains(coord) ||
          fullReservoirs.exists(r => r.contains(coord)) ||
          openRidge.contains(coord) ||
          current.contains(coord)

      if open.isEmpty && !current.isEmpty then
        rec(current :: fullReservoirs, closedRidge, openRidge, coordSet, coordSet)
      else if !open.isEmpty then
        val coord = open.head
        val height = grid(coord)

        if height == 9 then
          rec(fullReservoirs, closedRidge, openRidge + coord, current, open - coord)
        else
          val newNeighbours = coord.neighbours.filterNot(seen)
          val newOpen = (open ++ newNeighbours) - coord
          rec(fullReservoirs, closedRidge, openRidge, current + coord, newOpen)
      else if !openRidge.isEmpty then
        val coord = openRidge.head
        val newNeighbours = coord.neighbours.filterNot(seen)
        val (ridge, valley) = newNeighbours.partition(n => grid(n) == 9)
        if valley.length < 2 then
          rec(
            fullReservoirs,
            closedRidge + coord,
            (openRidge ++ ridge) - coord,
            current,
            open ++ valley
          )
        else rec(fullReservoirs, closedRidge, openRidge ++ ridge, current, open + valley.head)
      else current :: fullReservoirs

    rec(Nil, coordSet, coordSet, coordSet, Set(Coord(0, 0)))

  override def puzzle2(in: Stream[IO, String]): IO[String] =
    for lines <- in.map(line => line.map(ch => ch.toString.toInt)).compile.toVector
    yield
      val grid = Grids.load(lines)
      val result = reservoirs(grid).map(_.size).sorted.reverse.take(3).fold(1)(_ * _)
      s"multiplied, the three biggest reservoirs have size $result"
