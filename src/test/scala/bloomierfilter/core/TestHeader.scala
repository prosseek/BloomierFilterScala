package bloomierfilter.core

import org.scalatest.FunSuite

class TestHeader extends FunSuite {

  test ("Header simple") {
    val res = Array[Byte](43, 5)
    val h = Header(res)
    h.byteArray = res

    /*
       00101011:00000101
                -------- (5)
             --  (3)
       ------ (10)

      Makes : (5,3,10)
     */
    assert(h.m == 5)
    assert(h.k == 3)
    assert(h.Q == 8)
    assert(h.hashSeed == 10)
  }

  test("Header decode") {
    val h = new Header
    val res = Array[Byte](43, 5)
    val (m, qq, hashSeed) = h.decode(res)
    assert(m == 5)
    assert(qq == 8)
    assert(hashSeed == 10)
  }

  test("Head serialize") {
    val h:Header = new Header
    val res: Array[Byte] = h.serialize(m = 11, Q = 8, hashValue = 16)
    assert(res.mkString(":") == "67:11") // 1000011:(11 in decimal) => 16*4 + 3 (8 is stored in3) = 67
    val (m, qq, hashValue) = h.decode(res)
    assert(m == 11)
    assert(qq == 8)
    assert(hashValue == 16)
  }
}
