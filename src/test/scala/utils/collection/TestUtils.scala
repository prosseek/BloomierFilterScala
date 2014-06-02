package utils.collection

import org.scalatest.FunSuite

/**
 * Created by smcho on 5/28/14.
 */
class TestCollection extends FunSuite {
  test("noDuplication should return true with no duplication") {
    val t = List(1,2,3,4,5)
    assert(Utils.noDuplication(t) == true)
  }
  test("noDuplication should return false with duplication") {
    val t = List(1,2,2,4,5)
    assert(Utils.noDuplication(t) == false)
  }
  test("noDuplication is true with empty input") {
    val t = List()
    assert(Utils.noDuplication(t) == true)
  }
}
