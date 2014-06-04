package utils.crc

import org.scalatest._
import scala.collection.mutable.BitSet

/**
 * Created by smcho on 6/4/14.
 */
class TestCRC extends FunSuite {
  test ("CRC encode problem") {
    assert(CRC.encode(BitSet(2,1,0), 2).mkString(":") == "2:3:4")
    // from the example: http://en.wikipedia.org/wiki/Cyclic_redundancy_check
    var g = BitSet(3,1,0)
    assert(CRC.encode(BitSet(2,3,5,6,7,10,12,13), 3, Option(g)).mkString(":") == "2:5:6:8:9:10:13:15:16")
  }
  test ("CRC decode problem") {
    var decode = CRC.encode(BitSet(2,1,0), 2)
    assert(CRC.decode(decode,2) == true)
    var g = BitSet(3,1,0)
    decode = CRC.encode(BitSet(2,3,5,6,7,10,12,13), 3, Option(g))
    assert(CRC.decode(decode, 3, Option(g)) == true)
    decode = BitSet(0,2,5,6,8,9,10,13,15,16)
    assert(CRC.decode(decode, 3, Option(g)) == false)
  }
}
