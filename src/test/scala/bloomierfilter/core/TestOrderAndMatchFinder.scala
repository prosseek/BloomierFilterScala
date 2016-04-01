package bloomierfilter.core

import org.scalatest._

/**
 * Created by smcho on 6/2/14.
 */
class TestOrderAndMatchFinder extends FunSuite {

  test ("OrderAndMatchFinder simple check") {
    val keyMap = Map("abc"->10, "def"->20, "abd"->30)
    val m = 6
    val k = 3
    val o = new OrderAndMatchFinder(keysDict = keyMap, m = m, k = k, maxTry = 5, initialHashSeed = 0)
    o.find()

    /*
      012345 : m
      *  * * : "abc" (0,3,5)
        ***  : "abd" (2,3,4)
        * ** : "def" (2,4,5)

      "abc" has singleton at 0, and order 0
      "abd" has singleton at 3, and order 1
      "def" has singleton at 5, and order 2

 */
    assert(o.getNeighbors("abc") == List(0,3,5)) // 0th -> row 0 is selected
    assert(o.getNeighbors("abd") == List(2,3,4)) // 1st -> row 1 is selected
    assert(o.getNeighbors("def") == List(2,4,5)) // 0th -> row 2 is selected

    // We need to track of the hashSeed to get the same results
    // assert(o.hashSeed == 0)
    // println(o.depthCount)  // 2
    // println(o.orderHistory) // Map(1 -> ListBuffer(abc), 0 -> ListBuffer(abd, def))

    assert(o.piList == List("abd", "def", "abc"))
    assert(o.tauList == List(1, 2, 0)) // tauList indicates the location in the k neighbor locations
  }
}
