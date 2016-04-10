package bloomierfilter.core

import bloomierfilter.main.ByteArrayBloomierFilter
import chitchat.types.range
import org.scalatest.FunSuite
import util.conversion.ByteArrayTool

import scala.collection.mutable.{Map => MMap}
import java.lang.{String => JString}
import scala.{Byte => SByte}


class TestTable extends FunSuite {

  var Q = 10
  var valueString = new chitchat.types.String
  var valueAge = new range.Age

  def makeSimple = {
    // simple map setup

    var simpleMap = MMap[JString, Array[SByte]]()

    var byteArray = valueString.encode("Hello").get
    simpleMap("string") = ByteArrayTool.zeroPatch(byteArray, Q)

    byteArray = valueAge.encode(43).get
    simpleMap("age") = ByteArrayTool.zeroPatch(byteArray, Q)

    simpleMap
  }

  test ("serialize test") {
    val simpleMap = makeSimple
    // retrieving data
    val bbf = new ByteArrayBloomierFilter(input = simpleMap.toMap, initialm = 0, k = 3, q = Q * 8, initialHashSeed = 0)
    val table = bbf.table

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

  test("serialize & create table test") {

    // 1. create the map
    val simpleMap = makeSimple
    val bbf = new ByteArrayBloomierFilter(input = simpleMap.toMap, initialm = 0, k = 3, q = Q * 8, initialHashSeed = 0)
    val table = bbf.table
    val serialize = table.serialize

    // 2. serialize the table
    assert(serialize.size == 1 + Q*2)
    assert(table.serialize.mkString(":") == "5:47:26:79:105:-123:-52:-50:-21:-13:-11:9:82:36:10:22:-29:-112:-62:-54:-1")

    // 3. create table from byte array
    table.createTable(serialize)

    // 4. get the value from the stored table
    var value1 = bbf.getByteArray("string").get
    var value2 = bbf.getByteArray("age").get
    assert("Hello" == valueString.decode(value1).get)
    assert(43 == valueAge.decode(value2).get)

    // if the serialized data is wrong, it will break the decoder
    serialize(0) = 0
    table.createTable(serialize)

    intercept[java.util.NoSuchElementException] {
      value1 = bbf.getByteArray("string").get
      value2 = bbf.getByteArray("age").get
    }
    assert("Hello" == valueString.decode(value1).get)
    assert(43 == valueAge.decode(value2).get)
  }
}
