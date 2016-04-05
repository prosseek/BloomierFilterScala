package bloomierfilter.main

import org.scalatest.FunSuite

class TestBloomierFilterObject  extends FunSuite {
  test ("BloomierFilter/adjustByteArray patching") {
    val input = Array[Byte](0, 1, 2, 3, 4)
    val res = bloomierfilter.conversion.Util.zeroPatchByteArray(key = "Hello", value = input, Q = 10)
    assert("0:1:2:3:4:0:0:0:0:0" == res("Hello").mkString(":"))
  }
}
