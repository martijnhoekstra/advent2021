package advent

import cats._
import cats.effect._
import fs2._
import cats.syntax.all._

import scala.collection.immutable.BitSet

object Ages:
  val bornAge = 8
  val resetAge = 6

  opaque type AgeMap = Vector[Long]
  object AgeMap:
    def fromMap(ageMap: Map[Int, Long]): AgeMap =
      val max = ageMap.keySet.max
      (0 to bornAge).map(n => ageMap.get(n).getOrElse(0L)).toVector

  extension (am: AgeMap)
    def toMap = am.zipWithIndex.map { case (n, a) => a -> n }.toMap
    def format = am.zipWithIndex.map { case (n, a) => a -> n }.sortBy(_._1).map(_._2).toString

  def nextDay(ages: AgeMap): AgeMap = Vector.tabulate(bornAge + 1) {
    case `resetAge` => ages(0) + ages(resetAge + 1)
    case `bornAge`  => ages(0)
    case n          => ages(n + 1)
  }

object Day6LanternFish extends IOApp.Simple:
  import Ages._

  override def run: IO[Unit] =
    val input = "/input.txt"
      .resourceLines[IO]
      .flatMap(line => Stream.emits(line.split(',')))

    // and if you won't believe it, it's foldMap once again
    val ages = input.foldMap(age => Map(age.toInt -> 1L)).map(AgeMap.fromMap)
    val progression = ages.flatMap(agemap =>
      Stream.unfoldEval(agemap)(current => {
        val next = nextDay(current)
        IO(println(current.format)).as(Some(current -> nextDay(current)))
      })
    )

    val day80 = progression.take(257).compile.last

    for {
      age <- day80
      total = age.toList.flatMap(_.toMap.toList).foldMap(_._2)
      _ <- IO(println(s"there are $total fish"))
    } yield ()
