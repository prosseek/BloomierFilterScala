package core

import org.scalatest._

/**
 * Created by smcho on 6/1/14.
 */
class TestSingletonFindingTweaker extends FunSuite {

  test ("tweak tests m = 10, k = 3") {
    /*
      "abc" -> List(1, 7, 8)
      "def" -> List(4, 5, 7)
      "abd" -> List(0, 2, 4)

      nonSingleton (duplicated) = Set(7,4)
     */
    val b = new BloomierHasher(m = 10, k = 3, q = 32)
    val k = Map("abc"->10, "def"->20, "abd"->30)
    val t = new SingletonFindingTweaker(k, b)
    var n = t.tweak("abc")
    assert(n == 0) // First singleton is 0th (1)
    n = t.tweak("def")
    assert(n == 1)
    n = t.tweak("abd")
    assert(n == 0)
  }

}
