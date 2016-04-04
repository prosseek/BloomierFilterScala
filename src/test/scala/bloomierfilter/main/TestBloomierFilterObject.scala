package bloomierfilter.main

import org.scalatest.FunSuite

class TestBloomierFilterObject  extends FunSuite {
  test ("BloomierFilter/adjustByteArray") {
    val input = Array[Byte](0,1,2,3,4)
    val res = BloomierFilter.adjustByteArray(key = "Hello", value = input, Q = 2)

//    res foreach {
//      case (key, value) => {
//        println(key)
//        println(value.mkString(":"))
//      }
//    }
//
//    Hello2
//    0:4
//    Hello1
//    2:3
//    Hello
//    0:1

    assert("0:1" == res("Hello").mkString(":"))
    assert("2:3" == res("Hello1").mkString(":"))
    assert("0:4" == res("Hello2").mkString(":"))
  }

  test ("BloomierFilter/adjustByteArray patching") {
    val input = Array[Byte](0, 1, 2, 3, 4)
    val res = BloomierFilter.adjustByteArray(key = "Hello", value = input, Q = 10)
    assert("0:0:0:0:0:0:1:2:3:4" == res("Hello").mkString(":"))
  }
}
