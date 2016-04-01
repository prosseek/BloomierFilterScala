package bloomierfilter.main

import org.scalatest._
import scala.collection.mutable.{Map => MMap}
class TestByteArrayBloomierFilter extends FunSuite with BeforeAndAfter{
  var map = MMap[String, Array[Byte]]()
  var valueString: chitchat.types.String = _
  var Q = 10

  before {
    valueString = new chitchat.types.String
    val byteArray = valueString.encode("Hello")
    map("string") = util.conversion.ByteArrayTool.zeroPatch(byteArray, Q)
  }

  test("simple") {
    val bbf = new ByteArrayBloomierFilter(input = map.toMap, m = 5, k = 3, q = Q*8)
    val value = bbf.getByteArray("string").get
    assert("5:72:101:108:108:111:0:0:0:0" == value.mkString(":"))
    assert("Hello" == valueString.decode(value).get)
  }
}