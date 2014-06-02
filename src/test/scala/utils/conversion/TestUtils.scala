package utils.conversion

import org.scalatest._

/**
 * Created by smcho on 6/2/14.
 */
class TestUtils extends FunSuite {
  test ("ByteArrayXor test") {
    var x = Array[Byte](-1,-1,-1,-1,-1)
    var y = Array[Byte](-1,-1,-1,-1,-1)
    var res = Utils.byteArrayXor(x, y)
    assert(res.mkString == "00000")

    // 0xaa == 10101010 ^ 0x55 == 01010101 => 0xFFFF (-1)
    x = Array[Byte](0xaa.toByte, 0xaa.toByte, 0xaa.toByte, 0xaa.toByte)
    y = Array[Byte](0x55.toByte, 0x55.toByte, 0x55.toByte, 0x55.toByte)
    res = Utils.byteArrayXor(x, y)
    assert(res.mkString == "-1-1-1-1")
  }
}
