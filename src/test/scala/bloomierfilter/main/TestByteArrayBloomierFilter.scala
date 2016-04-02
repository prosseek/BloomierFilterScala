package bloomierfilter.main

import org.scalatest._
import scala.collection.mutable.{Map => MMap}
import java.lang.{String => JString}
import scala.{Byte => SByte}

import util.conversion.ByteArrayTool
import chitchat.types._

class TestByteArrayBloomierFilter extends FunSuite with BeforeAndAfter{
  var map = MMap[JString, Array[SByte]]()

  var Q = 10

  before {
  }

  test("simple") {
    // creating table
    var valueString = new chitchat.types.String
    var byteArray = valueString.encode("Hello")
    map("string") = ByteArrayTool.zeroPatch(byteArray, Q)

    var valueAge = new range.Age
    byteArray = valueAge.encode(43)
    map("age") = ByteArrayTool.zeroPatch(byteArray, Q)

    // retrieving data
    val bbf = new ByteArrayBloomierFilter(input = map.toMap, initialm = 5, k = 3, q = Q*8, initialHashSeed = 0)
    val value1 = bbf.getByteArray("string").get
    assert("5:72:101:108:108:111:0:0:0:0" == value1.mkString(":"))
    assert("Hello" == valueString.decode(value1).get)

    val value2 = bbf.getByteArray("age").get
    assert("43:0:0:0:0:0:0:0:0:0" == value2.mkString(":"))
    assert(43 == valueAge.decode(value2).get)

    // size check
    //assert(bbf.size == 21) // 2*Q(10) + 1 == 21
    // other parameter check
    println(bbf.m)
    println(bbf.Q)
    println(bbf.hashSeed)
  }
}