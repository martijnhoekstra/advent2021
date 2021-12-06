package advent

import cats._

import scala.collection.immutable.BitSet

object Coords:
  val bitlen = 10
  opaque type Coord = Int
  object Coord:
    def apply(x: Int, y: Int): Coord = x << bitlen | y
  extension (c: Coord)
    def x = c >> bitlen
    def y = c & ((1 << bitlen) - 1)
    def format = s"${c.x},${c.y}"

  case class Line(start: Coord, end: Coord):
    val points: Set[Coord] = if start.x == end.x then
      val min = math.min(start.y, end.y)
      val max = math.max(start.y, end.y)
      (min to max).map(y => Coord(start.x, y)).to(BitSet)
    else if start.y == end.y then
      val min = math.min(start.x, end.x)
      val max = math.max(start.x, end.x)
      (min to max).map(x => Coord(x, start.y)).to(BitSet)
    else
      val xdir = if start.x <= end.x then 1 else -1
      val ydir = if start.y <= end.y then 1 else -1
      val init = Iterator
        .unfold(start)(prev =>
          Option.when(prev != end) {
            val next = Coord(prev.x + xdir, prev.y + ydir)
            (prev, next)
          }
        )
        .to(BitSet)
      init + end

  /* The set of coordinates where they intersect.
   *
   * You could optimize by making points a lazy val and then first checking if it could at all
   * intersect by checking whether they have points in the same bounding box, but it's plenty fast like this
   */
  def intersections(line1: Line, line2: Line): Set[Coord] = line1.points.intersect(line2.points)
