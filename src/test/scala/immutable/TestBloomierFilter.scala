package immutable

import org.scalatest._

/**
 * Created by smcho on 6/2/14.
 */
class TestBloomierFilter extends FunSuite {
  test ("Simple bloomier filter") {
    val inputMap = Map[String, Int]("abc"->10, "def"->20, "abd"->30)
    val b = new BloomierFilter(inputMap, m = 6, k = 3, q = 8*4)
    assert(b.get("abc").getOrElse(-1) == 10)
    assert(b.get("def").getOrElse(-1) == 20)
    assert(b.get("abd").getOrElse(-1) == 30)
    //println(b.get("xyzab"))
  }
  test ("checkZeroTableElement test") {
    val x = Array[Byte](1,2,3)
    val y = Array[Byte](0,0,0)
    assert(BloomierFilter.checkZeroElement(x) == false)
    assert(BloomierFilter.checkZeroElement(y) == true)
  }
  test ("checkAllZeroElementsInTable test") {
    val neighbors1 = Array[Int](0,1,2)
    val neighbors2 = Array[Int](0,1,3)
    val table = Array(Array[Byte](0,0,0),Array[Byte](0,0,0),Array[Byte](0,0,0),Array[Byte](0,0,1))
    assert(BloomierFilter.checkAllZeroElementsInTable(neighbors1, table) == true)
    assert(BloomierFilter.checkAllZeroElementsInTable(neighbors2, table) == false)
  }
}

//def checkAllZeroElements(neighbors: Seq[Int]) : Boolean = {
//for (n <- neighbors) {
//if (!checkZeroTableElement(table(n))) return false
//}
//true
//}