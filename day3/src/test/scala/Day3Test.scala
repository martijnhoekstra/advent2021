package advent

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import cats.effect.IO
import munit.CatsEffectSuite

class IntegerSuite extends CatsEffectSuite with ScalaCheckSuite:
  import Day3Diagnostics._
  import Day3Diagnostics.pats._

  def genPat(i: Int) = Gen.listOfN(i, Gen.oneOf('0', '1')).map(x => parsePattern(x.mkString))
  def genPats(i: Int) = Gen.listOf(genPat(i))
  given Arbitrary[List[BitPattern]] = Arbitrary(genPats(patternLength))

  test("the oxygen example works as advertised") {
    val patterns = "/testinput.txt".resourceLines[IO].map(parsePattern).compile.toList
    val o21 = Set("11110", "10110", "10111", "10101", "11100", "10000", "11001").map(parsePattern)
    val o22 = Set("10110", "10111", "10101", "10000").map(parsePattern)
    val o23 = Set("10110", "10111", "10101").map(parsePattern)
    val o24 = Set("10110", "10111").map(parsePattern)
    val o25 = Set("10111").map(parsePattern)

    val expectedPatterns = List(o25, o24, o23, o22, o21)
    val expectedStringPatterns = expectedPatterns.map(_.map(_.stringPattern))
    for {
      pats <- patterns
      steps = applyBitCriteria(pats, oxygenBitCriterium.curried)
      result = steps.head.head
    } yield {
      assertEquals(
        steps.init.map(_.toSeq.map(_.stringPattern).toSet),
        expectedStringPatterns
      )
      assertEquals(steps.last.toSet, pats.toSet)
      assertEquals(result.toInt, 23)
    }
  }

  test("the co2 example works as advertised") {
    val patterns = "/testinput.txt".resourceLines[IO].map(parsePattern).compile.toList
    val o21 = Set("00100", "01111", "00111", "00010", "01010").map(parsePattern)
    val o22 = Set("01111", "01010").map(parsePattern)
    val o23 = Set("01010").map(parsePattern)
    val o24 = Set("01010").map(parsePattern)
    val o25 = Set("01010").map(parsePattern)

    val expectedPatterns = List(o25, o24, o23, o22, o21)
    val expectedStringPatterns = expectedPatterns.map(_.map(_.stringPattern))
    for {
      pats <- patterns
      steps = applyBitCriteria(pats, co2BitCriterium.curried)
      result = steps.head.head.toInt
    } yield {
      assertEquals(
        steps.take(patternLength).map(_.toSeq.map(_.stringPattern).toSet),
        expectedStringPatterns
      )
      assertEquals(result, 10)
    }
  }

  property("one more steps as there are columns") {
    forAll { (pats: List[BitPattern]) =>
      {
        val o2 = applyBitCriteria(pats, oxygenBitCriterium.curried)
        val co2 = applyBitCriteria(pats, co2BitCriterium.curried)
        assertEquals(o2.length, patternLength + 1)
        assertEquals(co2.length, patternLength + 1)
      }
    }
  }

  property("o2 sizes at most double") {
    forAll { (pats: List[BitPattern]) =>
      {
        val o2 = applyBitCriteria(pats, oxygenBitCriterium.curried)
        o2.sliding(2).foreach {
          case Seq(p1, p2) => assert(p2.length <= p1.length * 2)
          case _           => assert(true)
        }
      }
    }
  }

  property("co2 sizes at least double") {
    forAll { (pats: List[BitPattern]) =>
      {
        def printable = pats.map(_.stringPattern)
        val o2 = applyBitCriteria(pats, co2BitCriterium.curried).init
        o2.sliding(2).foreach {
          case Seq(p1, p2) if p2.length > 1 => assert(p2.length >= p1.length * 2, printable -> o2.map(_.map(_.stringPattern)))
          case _                             => assert(true)
        }
      }
    }
  }
