package core

/**
 * Created by smcho on 6/14/14.
 */
import org.scalatest._
class TestContext extends FunSuite {
  test ("Size test simple") {
    val key = "KEY VALUE f"
    val a = Map[String, Any]("KEY VALUE f" -> 12.345F)
    val b = new Context(a)
    assert(b.size() == (key.size + 4, key.size, 1*4))
  }
  test ("Size test") {
    val a = Map[String, Any]("KEY VALUE f" -> 12.345F, "URGENT idea s" -> "good",
      "VALUE i" -> 1234, "LATITUDE" -> 38.4343434F, "ALTITUDE" -> -0.00343F,
      "DATE" -> 20140614, "TIME" -> 03254333, "TEMPERATURE" -> -23.45F)
    val b = new Context(a)
    assert(b.size() == (98, 66, 4*8))
  }
}
