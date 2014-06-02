package core

import org.scalatest._

/**
 * Created by smcho on 6/2/14.
 */
class TestOrderAndMatchFinder extends FunSuite {

  test ("OrderAndMatchFinder simple check") {
    val keyMap = Map("abc"->10, "def"->20, "abd"->30)
    val m = 6
    val k = 3
    val q = 5
    val o = new OrderAndMatchFinder(keysDict = keyMap, m = m, k = k, q = q, maxTry = 5, initialHashSeed = 0)
    o.find()

    assert(o.piList == List("abc","abd","def"))
    assert(o.tauList == List(0,1,0))

    assert(o.getNeighbors("abc") == List(0,3,4)) // 0th -> row 0 is selected
    assert(o.getNeighbors("abd") == List(0,1,4)) // 1st -> row 1 is selected
    assert(o.getNeighbors("def") == List(2,3,4)) // 0th -> row 2 is selected
  }
}
