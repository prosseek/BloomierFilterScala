package utils.hash

import org.scalatest._
import scala.util.Random

class TestHash extends FunSuite {
  // getUnsignedHash tests
  test("getUnsignedHash return value should be less than maxValue") {

    for (i <- 1 to 100) {
      val maxValue = Random.nextInt(500) + 1
      val result = Hash.getUnsignedHash("Hash values", maxValue)
      assert(result < maxValue && result >= 0, "result (" + result +  ") is more than maxValue (" + maxValue + ")")
    }
  }

  test("getUnsignedHash should return same value with the same input") {
    val r1 = Hash.getUnsignedHash("ABC", 100)
    val r2 = Hash.getUnsignedHash("ABC", 100)
    assert(r1 == r2)
  }

  test("getUnsignedHash should raise an error with maxVal == 0") {
    intercept[CustomException] {
      Hash.getUnsignedHash("Hash values", 0)
    }
  }
  test("getUnsignedHash should raise an error with null string") {
    intercept[CustomException] {
      Hash.getUnsignedHash("", 10)
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
    import utils.collection.Utils

    for (i <- 1 to 100) {
      val count = Random.nextInt(20)
      val maxValue = count + 20
      val result = Hash.getUniqueHashes("Hash values, count, maxVal", count, maxValue)
      assert(result.size == count)
      assert(Utils.noDuplication(result) == true)
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

