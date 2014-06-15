package utils.conversion

import org.scalatest._
import scala.collection.BitSet

/**
 * Created by smcho on 5/31/14.
 */
class TestByteArray extends FunSuite {

  // double
  test ("double to byte array and back test ") {
    var value = 12.34
    assert(value == ByteArray.byteArrayToDouble(ByteArray.doubleToByteArray(value)))
    value = 1234.56
    assert(value == ByteArray.byteArrayToDouble(ByteArray.doubleToByteArray(value)))
    value = 0.001234
    assert(value == ByteArray.byteArrayToDouble(ByteArray.doubleToByteArray(value)))
    value = 0.0
    assert(value == ByteArray.byteArrayToDouble(ByteArray.doubleToByteArray(value)))
  }
  test ("double to byte array and back test (with size given) ") {
    var value = 12.34
    var size = 30
//    assert(value == ByteArray.byteArrayToDouble(ByteArray.doubleToByteArray(value, size), size))
//    value = 1234.56
//    assert(value == ByteArray.byteArrayToDouble(ByteArray.doubleToByteArray(value, size), size))
//    value = 0.001234
//    assert(value == ByteArray.byteArrayToDouble(ByteArray.doubleToByteArray(value, size), size))
//    value = 0.0
//    assert(value == ByteArray.byteArrayToDouble(ByteArray.doubleToByteArray(value, size), size))
  }

  // float
  test ("float to byte array and back test ") {
    var value = 12.34F
    assert(value == ByteArray.byteArrayToFloat(ByteArray.floatToByteArray(value)))
    value = 1234.56F
    assert(value == ByteArray.byteArrayToFloat(ByteArray.floatToByteArray(value)))
    value = 0.001234F
    assert(value == ByteArray.byteArrayToFloat(ByteArray.floatToByteArray(value)))
    value = 0.0F
    assert(value == ByteArray.byteArrayToFloat(ByteArray.floatToByteArray(value)))
  }
  test ("float to byte array and back test (with size given)") {
    var value = 12.34F
    var size = 100
//    assert(value == ByteArray.byteArrayToFloat(ByteArray.floatToByteArray(value, size), size))
//    value = 1234.56F
//    assert(value == ByteArray.byteArrayToFloat(ByteArray.floatToByteArray(value, size), size))
//    value = 0.001234F
//    assert(value == ByteArray.byteArrayToFloat(ByteArray.floatToByteArray(value, size), size))
//    value = 0.0F
//    assert(value == ByteArray.byteArrayToFloat(ByteArray.floatToByteArray(value, size), size))
  }

  // byte
  test ("byte to byte array and back test ") {
    var value : Byte = 0
    assert(value == ByteArray.byteArrayToByte(ByteArray.byteToByteArray(value)))
    value = Byte.MaxValue
    assert(value == ByteArray.byteArrayToByte(ByteArray.byteToByteArray(value)))
    value = Byte.MinValue
    assert(value == ByteArray.byteArrayToByte(ByteArray.byteToByteArray(value)))
    value = 123
    assert(value == ByteArray.byteArrayToByte(ByteArray.byteToByteArray(value)))
    value = -123
    assert(value == ByteArray.byteArrayToByte(ByteArray.byteToByteArray(value)))
  }
  test ("byte to byte array and back test (with size given)") {
    var value : Byte = 0
    var size : Int = 50
//    assert(value == ByteArray.byteArrayToByte(ByteArray.byteToByteArray(value, size), size))
//    value = Byte.MaxValue
//    assert(value == ByteArray.byteArrayToByte(ByteArray.byteToByteArray(value, size), size))
//    value = Byte.MinValue
//    assert(value == ByteArray.byteArrayToByte(ByteArray.byteToByteArray(value, size), size))
//    value = 123
//    assert(value == ByteArray.byteArrayToByte(ByteArray.byteToByteArray(value, size), size))
//    value = -123
//    assert(value == ByteArray.byteArrayToByte(ByteArray.byteToByteArray(value, size), size))
  }

  // int
  test ("int to byte array and back test ") {
    var value = 0
    assert(value == ByteArray.byteArrayToInt(ByteArray.intToByteArray(value)))
    value = Int.MaxValue
    assert(value == ByteArray.byteArrayToInt(ByteArray.intToByteArray(value)))
    value = Int.MinValue
    assert(value == ByteArray.byteArrayToInt(ByteArray.intToByteArray(value)))
    value = 12343434
    assert(value == ByteArray.byteArrayToInt(ByteArray.intToByteArray(value)))
  }
  test ("int to byte array and back test (with size given)") {
    var value = 0
    var size = 100
//    assert(value == ByteArray.byteArrayToInt(ByteArray.intToByteArray(value, size), size))
//    value = Int.MaxValue
//    assert(value == ByteArray.byteArrayToInt(ByteArray.intToByteArray(value, size), size))
//    value = Int.MinValue
//    assert(value == ByteArray.byteArrayToInt(ByteArray.intToByteArray(value, size), size))
//    value = 12343434
//    assert(value == ByteArray.byteArrayToInt(ByteArray.intToByteArray(value, size), size))
  }

  // long
  test ("long to byte array and back test ") {
    var value = 0L
    assert(value == ByteArray.byteArrayToLong(ByteArray.longToByteArray(value)))
    value = Long.MaxValue
    assert(value == ByteArray.byteArrayToLong(ByteArray.longToByteArray(value)))
    value = Long.MinValue
    assert(value == ByteArray.byteArrayToLong(ByteArray.longToByteArray(value)))
    value = 12343434L
    assert(value == ByteArray.byteArrayToLong(ByteArray.longToByteArray(value)))
  }
  test ("long to byte array and back test (with size given)") {
    var value = 0L
    var size = 111
//    assert(value == ByteArray.byteArrayToLong(ByteArray.longToByteArray(value, size), size))
//    value = Long.MaxValue
//    assert(value == ByteArray.byteArrayToLong(ByteArray.longToByteArray(value, size), size))
//    value = Long.MinValue
//    assert(value == ByteArray.byteArrayToLong(ByteArray.longToByteArray(value, size), size))
//    value = 12343434L
//    assert(value == ByteArray.byteArrayToLong(ByteArray.longToByteArray(value, size), size))
  }

  // string
  test ("string to byte array and back test") {
    var value = "Hello, world"
    assert(value == ByteArray.byteArrayToString(ByteArray.stringToByteArray(value, 10)))
    // even though the buffer size is bigger, the return value should be OK
    assert(value == ByteArray.byteArrayToString(ByteArray.stringToByteArray(value, 100)))
    assert("He" == ByteArray.byteArrayToString(ByteArray.stringToByteArray(value, 2)))
  }
  test("bitSet to bytearray test") {
    var x = BitSet(0,1,2,3,8,10,104)
    var y = ByteArray.bitSetToByteArray(x)
    assert(y.mkString(":") == "15:5:0:0:0:0:0:0:0:0:0:0:0:1")
    assert(ByteArray.byteArrayToBitSet(y) == x)

    x = BitSet(0,1,2,3,4,5,6,7,8)
    y = ByteArray.bitSetToByteArray(x)
    assert(y.mkString(":") == "-1:1")
    assert(ByteArray.byteArrayToBitSet(y) == x)
  }
}
