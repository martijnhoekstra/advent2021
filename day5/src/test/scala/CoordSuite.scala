package advent

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import cats.effect.IO
import munit.CatsEffectSuite

class CoordSuite extends ScalaCheckSuite:
  import Day5Venting._
  import  Coords._
  def l(str: String) = pLine.parseAll(str).toOption.get

  import Day5Venting._

  test("reverse line also has points") {
    assert(!l("9,4 -> 3,4").points.isEmpty)
  }

  test("example 1") {
    val points = l("1,1 -> 3,3").points
    assert(points.size == 3)
  }

  test("all directions") {
    val l1 = l("1,1 -> 3,3")
    val l2 = l("3,3 -> 1,1")
    assertEquals(l1.points, l2.points)
    val l3 = l("1,3 -> 3,1")
    val l4 = l("3,1 -> 1,3")
    assertEquals(l3.points, l4.points)
  }


