package bloomierfilter.main

import org.scalatest._
import scala.collection.mutable.{Map => MMap}
import java.lang.{String => JString}
import scala.{Byte => SByte}

import chitchat.types._
import util.conversion.ByteArrayTool

class TestByteArrayBloomierFilter extends FunSuite with BeforeAndAfter{

  // simple map setup
  var valueAge = new range.Age
  var valueString = new chitchat.types.String

  def makeSimple(Q:Int = 10) = {
    var simpleMap = MMap[JString, Array[SByte]]()
    var byteArray = valueString.encode("Hello").get
    simpleMap("string") = ByteArrayTool.zeroPatch(byteArray, Q)
    byteArray = valueAge.encode(43).get
    simpleMap("age") = ByteArrayTool.zeroPatch(byteArray, Q)
    simpleMap
  }


  before {
  }

  test("simple when m is given") {
    // retrieving data
    val Q = 10
    val simpleMap = makeSimple(10)
    val bbf = new ByteArrayBloomierFilter(input = simpleMap.toMap, initialm = 5, k = 3, q = Q*8, initialHashSeed = 0)
    val value1 = bbf.getByteArray("string").get
    assert("5:72:101:108:108:111:0:0:0:0" == value1.mkString(":"))
    assert("Hello" == valueString.decode(value1).get)

    val value2 = bbf.getByteArray("age").get
    assert("43:0:0:0:0:0:0:0:0:0" == value2.mkString(":"))
    assert(43 == valueAge.decode(value2).get)

    // size check
    assert(bbf.size == 21) // 2*Q(10) + 1 == 21
    // other parameter check
    assert(bbf.m == 5)
    assert(bbf.Q == 10)
    assert(bbf.hashSeed == 0)
  }

  test("simple when m is 0") {
    // retrieving data
    val Q = 10
    val simpleMap = makeSimple(Q)

    val bbf = new ByteArrayBloomierFilter(input = simpleMap.toMap, initialm = 0, k = 3, q = Q*8, initialHashSeed = 0)
    val value1 = bbf.getByteArray("string").get
    assert("5:72:101:108:108:111:0:0:0:0" == value1.mkString(":"))
    assert("Hello" == valueString.decode(value1).get)

    val value2 = bbf.getByteArray("age").get
    assert("43:0:0:0:0:0:0:0:0:0" == value2.mkString(":"))
    assert(43 == valueAge.decode(value2).get)

    // size check
    assert(bbf.size == 21) // 2*Q(10) + 1 == 21
    // other parameter check
    assert(bbf.m == 4)
    assert(bbf.Q == 10)
    assert(bbf.hashSeed == 0)
  }

  test("serailize test") {
    val Q = 8
    val simpleMap = makeSimple(Q)
    val bbf = new ByteArrayBloomierFilter(input = simpleMap.toMap, q = Q*8)
    val bytes = bbf.serialize

    val bbf2 = ByteArrayBloomierFilter(bytes)
    val ba1 = bbf2.getByteArray("string").get
    val ba2 = bbf.getByteArray("string").get

    assert(valueString.decode(ba1).get == valueString.decode(ba2).get)
  }

  test("Save bytearray into file and load it back") {
    val Q = 8
    val simpleMap = makeSimple(Q)
    val bbf = new ByteArrayBloomierFilter(input = simpleMap.toMap, q = Q*8)
    bbf.save("./resources/test/scalasimplemap.babf")

    // load and check
    val bbf2 = ByteArrayBloomierFilter("./resources/test/scalasimplemap.babf")

    val ba1 = bbf2.getByteArray("string").get
    val ba2 = bbf.getByteArray("string").get

    assert(valueString.decode(ba1).get == valueString.decode(ba2).get)
  }

  test("Information test") {
    val Q = 8
    val expect = """Map(lToIndex -> Map(0 -> 0, 2 -> 1), k -> 3, keyToL -> Map(string -> 2, age -> 0), m -> 4, C -> 0, Q -> 8, seed -> 0)"""
    val simpleMap = makeSimple(Q)
    val bbf = new ByteArrayBloomierFilter(input = simpleMap.toMap, q = Q*8)
    //println(bbf.information)
    assert(bbf.information.toString == expect)
  }
}