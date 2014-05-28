package utils.hash

import org.scalatest._
import scala.util.Random

class TestHash extends FunSuite {
  test("getUnsignedHash return value should be less than maxValue") {

    for (i <- 1 to 100) {
      val maxValue = Random.nextInt(500)
      val result = Hash.getUnsignedHash("Hash values", maxValue)
      assert(result < maxValue && result >= 0)
    }
  }
}

