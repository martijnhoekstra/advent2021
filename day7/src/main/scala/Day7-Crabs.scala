package advent

import cats._
import cats.effect._
import fs2._
import cats.syntax.all._
import scala.collection.immutable.ArraySeq

object Day7Crabs extends IOApp.Simple:

  override def run: IO[Unit] =
    def splitInts(str: String) = str.split(',').map(_.toInt).toList
    val crabPositions = "/input.txt"
      .bytes[IO]
      .through(text.utf8.decode)
      .foldMonoid
      .flatMap(bulk => Stream.emits(splitInts(bulk)))

    val puzzle1 = for
      // the total absolute deviation (and thus amount of fuel burnt) is smaller than any other
      // around the median, hence, the median is the number we align our crabs to
      rawPositions <- crabPositions.compile.toVector
      positions = rawPositions.sorted
      medianIndices = {
        val m = (positions.length - 1) / 2
        if (positions.length % 2 == 1) then List(m) else List(m, m + 1)
      }
      median = medianIndices.map(i => positions(i))
      medianValue = median.sum.toDouble / median.length
      integerMedian = math.round(medianValue).toInt
      totalFuel = positions.foldMap(pos => math.abs(pos - integerMedian))
      _ <- IO(println(s"most fuel-efficient crab-alignment is $integerMedian"))
      _ <- IO(println(s"spending $totalFuel fuel"))
    yield ()

    val puzzle2 = {
      def triangularDistance(n: Int) = (n.toLong * (n + 1)) / 2
      for
        // the total absolute triangular deviation (and thus amount of fuel burnt) is smaller than any other
        // around the mean, hence, the mean is the number we align our crabs to
        rawPositions <- crabPositions.compile.toVector
        mean = rawPositions.sum / rawPositions.length
        totalFuel = rawPositions.foldMap(pos => triangularDistance(math.abs(pos - mean)))
        _ <- IO(println(s"most fuel-efficient crab-alignment is $mean"))
        _ <- IO(println(s"spending $totalFuel fuel"))
      yield ()
    }

    puzzle1 *> puzzle2
