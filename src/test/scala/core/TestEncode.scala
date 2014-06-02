package core

import org.scalatest._
/**
 * Created by smcho on 6/2/14.
 */
class TestEncode extends FunSuite {

  test ("Encode for integer value") {
    var e = Encode.encode("KEY_VALUE", 10, 5)
    assert("0:0:0:0:10" == e.mkString(":"))

    e = Encode.encode("KEY_VALUE", 10, 2)
    assert("0:10" == e.mkString(":"))

    e = Encode.encode("KEY_VALUE", -1, 5)
    assert("-1:-1:-1:-1:-1" == e.mkString(":"))

    e = Encode.encode("KEY_VALUE", -1, 2)
    assert("-1:-1" == e.mkString(":"))
  }

}
