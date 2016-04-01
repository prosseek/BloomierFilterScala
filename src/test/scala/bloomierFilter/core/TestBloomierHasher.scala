package core

import org.scalatest._

class TestBloomierHasher extends FunSuite {

  def noDuplication[T](list:List[T]) = {
    val size = list.length
    if (size == 0) true
    else {
      val set = list.toSet
      set.size == size
    }
  }

  test ("Bloomier Parameter") {
    val p = BloomierParameter(hashSeed = 0, m = 10, k = 20, q = 30)
    val b = new BloomierHasher(p)
    assert(b.hashSeed == 0)
    assert(b.m == 10)
    assert(b.k == 20)
    assert(b.q == 30)
  }

  test("getNeighborhood returns k unique values under m") {
    val p = BloomierParameter(hashSeed = 0, m = 10, k = 3, q = 32)
    val b = new BloomierHasher(p)
    val n = b.getNeighborhood("KEY1")
    assert(n.size == p.k)
    assert(noDuplication(n) == true)
  }

  test ("getM returns q/8 + 1 random data under 255") {
    val b = BloomierHasher() // default q = 4 bytes
    //println(b.getM("Hello, world"))
  }

  test ("getM should return the same value with the same input") {
    val b = BloomierHasher()
    val x = b.getM("Test1")
    val y = b.getM("Test1")
    assert(x == y)
  }

  test ("get byte should be x/8 + 1") {
    var b = BloomierHasher(q=0)
    assert(b.byteSize == 1)
    b = BloomierHasher(q=1)
    assert(b.byteSize == 1)
    b = BloomierHasher(q=8*5)
    assert(b.byteSize == 5)
    b = BloomierHasher(q=8*5 + 4)
    assert(b.byteSize == (5 + 1))
  }
}
