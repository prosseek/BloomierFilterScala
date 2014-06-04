package utils.conversion

import org.scalatest._
import scala.collection.BitSet

/**
 * Created by smcho on 5/31/14.
 */
class TestByteArray extends FunSuite {

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
  test ("byte to byte array and back test ") {
    var value : Byte = 0
    assert(value == ByteArray.byteArrayToByte(ByteArray.dataToByteArray(value)))
    value = Byte.MaxValue
    assert(value == ByteArray.byteArrayToByte(ByteArray.dataToByteArray(value)))
    value = Byte.MinValue
    assert(value == ByteArray.byteArrayToByte(ByteArray.dataToByteArray(value)))
    value = 123
    assert(value == ByteArray.byteArrayToByte(ByteArray.dataToByteArray(value)))
    value = -123
    assert(value == ByteArray.byteArrayToByte(ByteArray.dataToByteArray(value)))
  }
  test ("int to byte array and back test ") {
    var value = 0
    assert(value == ByteArray.byteArrayToInt(ByteArray.dataToByteArray(value)))
    value = Int.MaxValue
    assert(value == ByteArray.byteArrayToInt(ByteArray.dataToByteArray(value)))
    value = Int.MinValue
    assert(value == ByteArray.byteArrayToInt(ByteArray.dataToByteArray(value)))
    value = 12343434
    assert(value == ByteArray.byteArrayToInt(ByteArray.dataToByteArray(value)))
  }
  test ("long to byte array and back test ") {
    var value = 0L
    assert(value == ByteArray.byteArrayToLong(ByteArray.dataToByteArray(value)))
    value = Long.MaxValue
    assert(value == ByteArray.byteArrayToLong(ByteArray.dataToByteArray(value)))
    value = Long.MinValue
    assert(value == ByteArray.byteArrayToLong(ByteArray.dataToByteArray(value)))
    value = 12343434L
    assert(value == ByteArray.byteArrayToLong(ByteArray.dataToByteArray(value)))
  }
  test ("string to byte array and back test") {
    var value = "Hello, world"
    assert(value == ByteArray.byteArrayToString(ByteArray.stringToByteArray(value)))
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
