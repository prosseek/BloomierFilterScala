package bloomierfilter.core

import org.scalatest.FunSuite

class TestHeader extends FunSuite {

  test ("Header simple") {
    val res = Array[Byte](-85, 5) // -128 (high bit 1) + 43 => 85
    val h = Header(res)
    h.byteArray = res

    /*
       10101011:00000101
                -------- (5)
             --  (3)
        ----- (10)
       -      (0)

      Makes : (5,3,10)
     */
    assert(h.m == 5*4)
    assert(h.k == 3)
    assert(h.Q == 8)
    assert(h.hashSeed == 10)
    assert(h.complete == 1)
  }

  test("Header decode") {
    val h = new Header
    val res = Array[Byte](-85, 5)
    val (m, qq, hashSeed, complete) = h.decode(res)
    assert(m == 5*4)
    assert(qq == 8)
    assert(hashSeed == 10)
    assert(complete == 1)
  }

  test("Head serialize") {
    val h:Header = new Header
    val res: Array[Byte] = h.encode(m = 11, Q = 8, hashSeed = 16, complete = 0)
    assert(res.mkString(":") == "67:3") // 1000011:(11 in decimal) => 16*4 + 3 (8 is stored in3) = 67, 3*4 = 12, m is 4 fold
    val (m, qq, hashSeed, complete) = h.decode(res)
    assert(m == 12)
    assert(qq == 8)
    assert(hashSeed == 16)
    assert(complete == 0)
  }
}
