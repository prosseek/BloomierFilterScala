package bloomierfilter.core

import org.scalatest._

class TestSingletonFindingTweaker extends FunSuite {

  test ("tweak tests m = 6, k = 3") {
    /*
          012345 : m
          *  * * : "abc" (0,3,5)
            ***  : "abd" (2,3,4)
            * ** : "def" (2,4,5)

          "abc" has singleton at 0 -> location 0, and order 2
          ---------------------------------------------------
          "abd" has singleton at 3 -> location 1, and order 1
          "def" has singleton at 5 -> location 2, and order 1

          val o = new OrderAndMatchFinder(keysDict = keyMap, m = m, k = k, maxTry = 5, initialHashSeed = 0)
          o.find()

          println(o.piList)   // List(abd, def, abc)
          println(o.tauList)  // List(1, 2, 0)

     */
    val m = 6
    val k = 3
    val q = 32

    // setup
    val b = new BloomierHasher(m = m, k = k, q = q)
    val keyMap = Map("abc"->10, "def"->20, "abd"->30)
    val t = new SingletonFindingTweaker(keyMap.keys.toList, b)

    var n = t.tweak("abc")
    assert(n == 0)

    n = t.tweak("def")
    assert(n == -1)

    n = t.tweak("abd")
    assert(n == -1)
  }

  test ("tweak tests m = 10, k = 3") {
    /*
          0123456789 : m
           *  * *    : "abc" (1,4,6)
          **  *      : "abd" (0,1,4)
          *    *  *  : "def" (0,5,8)

          "abc" has singleton at 6 -> 2nd location in k
          "def" has singleton at 5 -> 0th location in k
          -----------------------------------------------------
          "abd" has singleton at 5 -> 0th location

          val o = new OrderAndMatchFinder(keysDict = keyMap, m = m, k = k, maxTry = 5, initialHashSeed = 0)
          o.find()
          println(o.piList)   // List(abd, abc, def)
          println(o.tauList)  // List(0,   2,   1)

     */
    val m = 10
    val k = 3
    val q = 32

    // setup
    val b = new BloomierHasher(m = m, k = k, q = q)
    val keyMap = Map("abc"->10, "def"->20, "abd"->30)
    val t = new SingletonFindingTweaker(keyMap.keys.toList, b)

    var n = t.tweak("abc")
    assert(n == 2)    // 3rd (index 2) in the (1,4,6) is singleton

    n = t.tweak("abd") // 2nd (index 1) in the (0,5,8) is singleon
    assert(n == -1)

    n = t.tweak("def") // cannot find singleton first time, so -1
    assert(n == 1)
  }

  test ("tweak tests m = 20, k = 3") {
    /*
          0123456789 : m
           *  * *    : "abc" (1,4,6)
          **  *      : "abd" (0,1,4)
          *    *  *  : "def" (0,5,8)

          "abc" has singleton at 6 -> 2nd location in k
          "def" has singleton at 5 -> 0th location in k
          -----------------------------------------------------
          "abd" has singleton at 5 -> 0th location

          val o = new OrderAndMatchFinder(keysDict = keyMap, m = m, k = k, maxTry = 5, initialHashSeed = 0)
          o.find()
          println(o.piList)   // List(abd, abc, def)
          println(o.tauList)  // List(0,   2,   1)

     */
    val m = 20
    val k = 3
    val q = 32

    // setup
    val keys = List("abc", "abd", "def")
    var b = new BloomierHasher(m = m, k = k, q = q, hashSeed = 0)

    assert(b.getNeighborhood("abc") == List(1, 6, 11))
    assert(b.getNeighborhood("abd") == List(0, 1, 4))
    assert(b.getNeighborhood("def") == List(5, 10, 18))

    var t = new SingletonFindingTweaker(keys, b)
    assert(t.tweak("abc") == 1)
    assert(t.tweak("abd") == 0)
    assert(t.tweak("def") == 0)

    b = new BloomierHasher(m = m, k = k, q = q, hashSeed = 2)
    t = new SingletonFindingTweaker(keys, b)
    assert(b.getNeighborhood("abc") == List(1, 6, 11))
    assert(b.getNeighborhood("abd") == List(1, 11, 13))
    assert(b.getNeighborhood("def") == List(1, 7, 18))
    assert(t.tweak("abc") == 1)
    assert(t.tweak("abd") == 2)
    assert(t.tweak("def") == 1)
  }
}
