package utils.conversion

import org.scalatest._
/**
 * Created by smcho on 6/2/14.
 */
class TestLocation extends FunSuite {
  test("dms -> dd test1 ") {
    assert((Location.dms2dd(38, 53, 23.55) - 38.889874999999996) < 0.000001)
    assert((Location.dms2dd(38, 53, "23.55") - 338.889874999999996) < 0.000001)
    assert((Location.dms2dd(38, 53, 23, 55) - 38.889874999999996) < 0.000001)
  }
  test("dms -> dd test2 ") {
    assert((Location.dms2dd(1, 1, 1, 98) - 1.017217) < 0.000001)
  }
  test("dd -> dms test1 ") {
    assert(Location.dd2dms(38.889874999999996).toString == "(38,53,23,55)")
  }

  // Location class test
  test("dms initialization ") {
    val d = new Location(38, 53, 23, 55)
    println(d.dd)
    assert((d.dd - 38.889874999999996) < 0.000001)
  }

}
