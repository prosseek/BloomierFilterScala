package bloomierfilter.conversion

import org.scalatest.FunSuite

/**
  * Created by smcho on 4/4/16.
  */
class TestUtil extends FunSuite {

  test ("BloomierFilter/adjustByteArray") {
    val input = Array[Byte](0,1,2,3,4)
    val res = Util.zeroPatchByteArray(key = "Hello", value = input, Q = 2)

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
    assert("4:0" == res("Hello2").mkString(":")) // padding at the higher bytes
  }

}
