package immutable

import org.scalatest._

/**
 * Created by smcho on 6/2/14.
 */
class TestBloomierFilter extends FunSuite {
  test ("Simple bloomier filter 3 depth") {
//    Map(2 -> ListBuffer(abd), 1 -> ListBuffer(hello), 0 -> ListBuffer(abc, def))
//    abc>> 3:5:6:
//    def>> 3:4:5:
//    abd>> 0:1:4:
//    hello>> 0:4:6:
//    OrderAndMatch(0,List(abc, def, hello, abd),List(2, 1, 0, 1))
    val inputMap = Map[String, Any]("abc"->10, "def"->20, "abd"->30, "hello"->null)
    val b = new BloomierFilter(inputMap, m = 10, k = 2, q = 8*4)
    assert(b.getDepth() == 1) // when depth is 1 it's type 1 contexts
    assert(b.get("abc").getOrElse(-1) == 10)
    assert(b.get("def").getOrElse(-1) == 20)
    assert(b.get("abd").getOrElse(-1) == 30)
    assert(b.get("hello").getOrElse(-1) == "Bottom") // hello should return Bottom, as it is given no input
  }
  test ("Same as before, but larger m") {
    //    Map(2 -> ListBuffer(abd), 1 -> ListBuffer(hello), 0 -> ListBuffer(abc, def))
    //    abc>> 3:5:6:
    //    def>> 3:4:5:
    //    abd>> 0:1:4:
    //    hello>> 0:4:6:
    //    OrderAndMatch(0,List(abc, def, hello, abd),List(2, 1, 0, 1))
    val inputMap = Map[String, Int]("abc" -> 10, "def" -> 20, "abd" -> 30, "hello" -> 40)
    val b = new BloomierFilter(inputMap, m = 28, k = 3, q = 8 * 4)
    assert(b.get("abc").getOrElse(-1) == 10)
    assert(b.get("def").getOrElse(-1) == 20)
    assert(b.get("abd").getOrElse(-1) == 30)
  }

  test ("Simple bloomier filter 2 depth") {

//    Map(1 -> ListBuffer(abd, def), 0 -> ListBuffer(abc))
//    abc>> 0:3:4:
//    def>> 2:3:4:
//    abd>> 0:1:4:
//    OrderAndMatch(0,List(abc, abd, def),List(0, 1, 0))

    val inputMap = Map[String, Int]("abc"->10, "def"->20, "abd"->30)
    val b = new BloomierFilter(inputMap, m = 4, k = 2, q = 8*4)
    assert(b.get("abc").getOrElse(-1) == 10)
    assert(b.get("def").getOrElse(-1) == 20)
    assert(b.get("abd").getOrElse(-1) == 30)
    //println(b.get("xyzab")) // Will return Some("Bottom") - Some(1811437596)
    //print(b.analyze(inputMap))
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