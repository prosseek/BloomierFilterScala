package utils.conversion

import org.scalatest._
/**
 * Created by smcho on 6/2/14.
 */
class TestPartialLocation extends FunSuite {
  test("dms -> dd test1 ") {
    assert((PartialLocation.dms2dd(38, 53, 23.55) - 38.889875) < 0.000001)
    assert((PartialLocation.dms2dd(38, 53, "23.55") - 338.889875) < 0.000001)
    assert((PartialLocation.dms2dd(38, 53, 23, 55) - 38.889875) < 0.000001)
  }
  test("dms -> dd test2 ") {
    assert((PartialLocation.dms2dd(1, 1, 1, 98) - 1.017217) < 0.000001)
  }
  test("dms -> dd test3 negative ") {
    assert((PartialLocation.dms2dd(-38, 53, 23, 55)  + 38.8898749) < 0.000001)
  }
  test("dd -> dms test1 ") {
    assert(PartialLocation.dd2dms(38.889875).toString == "(38,53,23,55)")
  }
  test("dd -> dms test2 ") {
    assert(PartialLocation.dd2dms(-38.889875).toString == "(-38,53,23,55)")
  }

  // Location class test
  test("dms initialization ") {
    val d = new PartialLocation(38, 53, 23, 55)
    //println(d.dd)
    assert((d.dd - 38.889874999999996) < 0.000001)
  }

  test("to byte array") {
    var d = new PartialLocation(38, 53, 23, 55)
    assert(d.toByteArray().mkString(":") == "38:-22:-21:6")
    d = new PartialLocation(-38, 53, 23, 55)
    assert(d.toByteArray().mkString(":") == "-38:-22:-21:6")
  }

  // byte array
  test("byte array and location test latitude") {
    val b = new PartialLocation(38, 53, 23, 55, t=PartialLocation.LATITUDE)
    val ba = b.toByteArray()
    //println(ba.mkString(":"))
    val b2 = new PartialLocation(ba, t=PartialLocation.LATITUDE)
    //println(b2.toBitSet().mkString(":"))
    assert(b2.getDms == b.getDms)
  }

  test("byte array and location test altitude") {
    val b = new PartialLocation(38, 53, 23, 55, t=PartialLocation.ALTITUDE)
    val ba = b.toByteArray()
    //println(ba.mkString(":"))
    val b2 = new PartialLocation(ba, t=PartialLocation.ALTITUDE)
    //println(b2.toBitSet().mkString(":"))
    assert(b2.getDms == b.getDms)
  }
}
