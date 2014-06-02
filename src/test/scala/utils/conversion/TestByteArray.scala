package utils.conversion

import org.scalatest._
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
}
