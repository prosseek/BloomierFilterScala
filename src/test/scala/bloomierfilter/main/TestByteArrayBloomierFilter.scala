package bloomierfilter.main

import org.scalatest._
import scala.collection.mutable.{Map => MMap}
import java.lang.{String => JString}
import scala.{Byte => SByte}

import util.conversion.ByteArrayTool
import chitchat.types._

class TestByteArrayBloomierFilter extends FunSuite with BeforeAndAfter{

  // simple map setup
  var Q = 10
  var simpleMap = MMap[JString, Array[SByte]]()
  var valueString = new chitchat.types.String
  var byteArray = valueString.encode("Hello")
  var valueAge = new range.Age
  simpleMap("string") = ByteArrayTool.zeroPatch(byteArray, Q)
  byteArray = valueAge.encode(43)
  simpleMap("age") = ByteArrayTool.zeroPatch(byteArray, Q)


  before {
  }

  test("simple when m is given") {
    // retrieving data
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
    assert(bbf.hashSeed == 1)
  }

  test("simple when m is 0") {
    // retrieving data
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
}