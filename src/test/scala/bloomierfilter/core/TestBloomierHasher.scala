package bloomierfilter.core

import org.scalatest._

class TestBloomierHasher extends FunSuite {

  test("getNeighborhood returns k unique values under m") {
    val b = new BloomierHasher(hashSeed = 0, m = 10, k = 3, q = 32)
    val n = b.getNeighborhood("KEY1")
    assert(n.size == 3)
    assert(util.Util.noDuplication(n) == true)
  }

  test ("getM returns q/8 + 1 random data under 255") {
    val b = new BloomierHasher() // default q = 4 bytes
    assert(b.getM("Hello, world") == List(48, 80, 105, 245))
  }

  test ("getM should return the same value with the same input") {
    val b = new BloomierHasher()
    val x = b.getM("Test1")
    val y = b.getM("Test1")
    assert(x == y)
  }

  test ("get byte should be x/8 + 1") {
    var b = new BloomierHasher(q=0)
    assert(b.byteSize == 1)
    b = new BloomierHasher(q=1)
    assert(b.byteSize == 1)
    b = new BloomierHasher(q=8*5)
    assert(b.byteSize == 5)
    b = new BloomierHasher(q=8*5 + 4)
    assert(b.byteSize == (5 + 1))
  }
}
