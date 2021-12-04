package advent

import fs2._
import cats._
import cats.effect._
import cats.syntax.all._
import scala.collection.immutable.ArraySeq

class BitPatterns(val patternLength: Int):
  opaque type BitPattern = ArraySeq[Boolean]
  opaque type BitPatternSum = ArraySeq[Int]

  def parseSum(line: String): BitPatternSum = line.iterator
    .map {
      case '0' => -1
      case '1' => 1
    }
    .to(ArraySeq)

  def parsePattern(line: String): BitPattern = line.iterator
    .map {
      case '0' => false
      case '1' => true
    }
    .to(ArraySeq)

  extension (sum: BitPatternSum)
    def toMostCommon: BitPattern = sum.map(_ >= 0)
    def toLeastCommon: BitPattern = sum.map(_ <= 0)

  extension (pat: BitPattern)
    def toInt = pat.foldLeft(0) { case (n, bit) => (n << 1) + (if bit then 1 else 0) }
    def toSeq: Seq[Boolean] = pat
    def stringPattern: String = pat.map(b => if b then '1' else '0').mkString
    def apply(i: Int) = pat(i)

  given Monoid[BitPatternSum] with
    override def empty = ArraySeq.fill(patternLength)(0)
    override def combine(s1: BitPatternSum, s2: BitPatternSum): BitPatternSum =
      s1.zip(s2).map { case (a, b) => a + b }

object Day3Diagnostics extends IOApp.Simple:
  val pats = new BitPatterns(12)
  import pats._

  def applyBitCriteria(
      patterns: List[BitPattern],
      bitCriteria: Int => (=> Boolean) => Boolean
  ): List[List[BitPattern]] =
    def rec(
        remaining: List[BitPattern],
        seen: List[BitPattern],
        balance: Int,
        col: Int,
        allow: (=> Boolean) => Boolean,
        result: List[List[BitPattern]]
    ): List[List[BitPattern]] = remaining match {
      case x if col >= patternLength => result
      case Nil =>
        seen match {
          case s @ List(p) => {
            val fillAmount = patternLength - col
            List.fill(fillAmount)(s) ::: result
          }
          case multiple => {
            rec(multiple, Nil, 0, col + 1, bitCriteria(balance), multiple :: result)
          }
        }
      case pattern :: tail =>
        if !(allow(pattern(col))) then rec(tail, seen, balance, col, allow, result)
        else {
          val balance1 =
            balance + (if col < (patternLength - 1) && pattern(col + 1) then 1 else -1)
          rec(tail, pattern :: seen, balance1, col, allow, result)
        }
    }
    rec(patterns, Nil, balance = 0, col = -1, _ => true, Nil)

  def oxygenBitCriterium(balance: Int, bit: => Boolean) =
    if (balance > 0) /* 1 is the most common, keep 1 */ then bit
    else if (balance < 0) /* 0 is the most common, keep 0 */ then !bit
    else /* equally common, keep 1 */ bit

  def co2BitCriterium(balance: Int, bit: => Boolean) =
    if (balance < 0) /* 1 is the least common, keep 1 */ then bit
    else if (balance > 0) /* 0 is the least common, keep 0 */ then !bit
    else /* equally common, keep 0 */ !bit

  def oxygenValue(patterns: List[BitPattern]) =
    applyBitCriteria(patterns, oxygenBitCriterium.curried).head.head
  def co2Value(patterns: List[BitPattern]) = applyBitCriteria(patterns, co2BitCriterium.curried).head.head

  override def run: IO[Unit] =
    for {
      sum <- "/input.txt".resourceLines[IO].foldMap(parseSum).compileSingleton
      gamma = sum.toMostCommon.toInt
      epsilon = sum.toLeastCommon.toInt
      _ <- IO(println(s"p1: ${gamma * epsilon}"))
      patterns <- "/input.txt".resourceLines[IO].map(parsePattern).compile.toList
      o2 = oxygenValue(patterns)
      co2 = co2Value(patterns)
      _ <- IO(println(s"o2: ${o2.stringPattern} = ${o2.toInt}"))
       _ <- IO(println(s"co2: ${co2.stringPattern} = ${co2.toInt}"))
       _ <- IO(println(s"p2: ${o2.toInt.toLong * co2.toInt.toLong}"))

    } yield ()