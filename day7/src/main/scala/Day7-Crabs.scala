package advent

import cats._
import cats.effect._
import fs2._
import cats.syntax.all._
import scala.collection.immutable.ArraySeq

object Day7Crabs extends AdventDay:

  override def puzzle1(lines: Stream[IO, String]) =
    // the total absolute deviation (and thus amount of fuel burnt) is smaller than any other
    // around the median, hence, the median is the number we align our crabs to
    for
      rawPositions <- lines
        .flatMap(line => {
          val numbers = line.split(',').map(_.toInt)
          Stream.chunk(Chunk.array(numbers))
        })
        .compile
        .toVector
    yield
      val positions = rawPositions.sorted
      val medianIndices = {
        val m = (positions.length - 1) / 2
        if (positions.length % 2 == 1) then List(m) else List(m, m + 1)
      }
      val median = medianIndices.map(i => positions(i))
      val medianValue = median.sum.toDouble / median.length
      val integerMedian = math.round(medianValue).toInt
      val totalFuel = positions.foldMap(pos => math.abs(pos - integerMedian))
      s"""|most fuel-efficient crab-alignment is $integerMedian
          |spending $totalFuel fuel""".stripMargin

  override def puzzle2(lines: Stream[IO, String]) =
    // if the triangular distance would be n^2, it would be the distance from the mean
    // but it's not, so it's not, and we hill-climb it
    def triangularDistance(n: Int) = (n * (n + 1)) / 2
    for
      rawPositions <- lines
        .flatMap(line => {
          val numbers = line.split(',').map(_.toInt)
          Stream.chunk(Chunk.array(numbers))
        })
        .compile
        .toVector
    yield
      val totalFuel = findMinimum(rawPositions, triangularDistance)
      s"""most fuel-efficient crab-alignment $totalFuel fuel""".stripMargin

  def findMinimum(positions: Vector[Int], distance: Int => Int) =
    def cost(position: Int) = positions.foldMap(pos => distance(math.abs(pos - position)))
    def rec(minPosition: Int, minDistances: Int, maxPosition: Int, maxDistances: Int): Int =
      if minPosition >= maxPosition then minDistances
      else
        val midPosition = minPosition + ((maxPosition - minPosition) / 2)
        val midCost = cost(midPosition)
        val loCost = cost(midPosition - 1)
        val hiCost = cost(midPosition + 1)
        if loCost < midCost then rec(minPosition, minDistances, midPosition - 1, loCost)
        else if hiCost < midCost then rec(midPosition + 1, hiCost, maxPosition, maxDistances)
        else midCost
    rec(positions.min, cost(positions.min), positions.max, cost(positions.max))
