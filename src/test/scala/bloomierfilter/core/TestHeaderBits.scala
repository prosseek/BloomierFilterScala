package bloomierfilter.core

import org.scalatest.FunSuite

class TestHeaderBits extends FunSuite {

  test ("HeaderBits simple") {

    val hb = new HeaderBits
    val res = hb.encode(Seq[Int](5, 3, 10, 0)).get

    /*
       00101011:00000101
                -------- (5)
             --  (3)
       ------ (10)

      Makes : (5,3,10)
     */

    assert(res.mkString(":") == "43:5")
    val res2 = hb.decode(res)
    assert(res2.get == List(5,3,10,0))
  }
}
