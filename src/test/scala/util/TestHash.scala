package util.hash

import org.scalatest._

import scala.util.Random
import util._

import scala.collection.mutable.ArrayBuffer

class TestHash extends FunSuite {

  def max(s1: Int, s2: Int): Int = if (s1 > s2) s1 else s2
  def min(s1: Int, s2: Int): Int = if (s1 < s2) s1 else s2

  test("getUnsignedHash return value should be less than maxValue") {
    val result = ArrayBuffer[Int]()


    val maxValue = 1000
    for (i <- 1 to 10000) {
      val seed = java.time.LocalTime.now.toString
      val value = Hash.getHash(seed, maxValue)

      result += value

      assert(value < maxValue && value >= 0,
        "result (" + value +  ") is more than maxValue (" + maxValue + ")")
    }

    val maxVal = result.reduceLeft(max)
    val minVal = result.reduceLeft(min)
  }

  test("getUnsignedHash should return same value with the same input") {
    val r1 = Hash.getHash("ABC", 100)
    val r2 = Hash.getHash("ABC", 100)
    assert(r1 == r2)
  }

  test("getUnsignedHash should raise an error with maxVal == 0") {
    intercept[RuntimeException] {
      Hash.getHash("Hash values", 0)
    }
  }
  test("getUnsignedHash should raise an error with null string") {
    intercept[RuntimeException] {
      Hash.getHash("", 10)
    }
  }

  // getHashes
  test("getHashes will generate count hash values") {
    for (i <- 1 to 100) {
      val count = Random.nextInt(20)
      val maxValue = Random.nextInt(500) + 1 // In order to prevent the value 0 as the maxValue
      val result = Hash.getHashes(name="Hash values", count=count, maxVal=maxValue)
      assert(result.size == count)
    }
  }

  // getUniqueHashes
  // Warnings: this **does not** cover many test cases.
  test("getUniqueHashes generates hashes without duplication") {

    for (i <- 1 to 100) {
      val count = Random.nextInt(20)
      val maxValue = count + 20
      val result = Hash.getUniqueHashes("Hash values, count, maxVal", count, maxValue)
      assert(result.size == count)
      assert(util.Util.noDuplication(result) == true)
    }
  }

  test("getUniqueHashes should return the same value with the same input") {
    val r1 = Hash.getUniqueHashes("Hello, world")
    var r2 = Hash.getUniqueHashes("Hello, world")
    assert(r1 == r2)
    r2 = Hash.getUniqueHashes(key = "Hello, world", count = 3, maxVal=123)
    assert(r1 != r2)
  }
}

