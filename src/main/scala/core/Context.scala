package core

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Context {
  def toInt(input: String): Option[Int] = {
    try Some(input.toInt)
    catch {
      case _: java.lang.NumberFormatException => None
    }
  }

  def toFloat(input: String): Option[Float] = {
    try Some(input.toFloat)
    catch {
      case _: java.lang.NumberFormatException => None
    }
  }

  def processValue(key: String, value: String, map: collection.mutable.Map[String, Any]) = {
    if (!toInt(value).isEmpty) map(key) = toInt(value).get
    else if (!toFloat(value).isEmpty) map(key) = toFloat(value).get
    else if (value == "null" || value == "NULL") map(key) = null
    else {
      map(key) = value
    }
  }

  def readContextsFromFile(directory: String, fileName: String) = {
    val source = Source.fromFile(directory + "/" + fileName, "UTF-8")
    val contexts = ArrayBuffer[Context]()

    val keysDict = collection.mutable.Map[String, Any]()
    for (l <- source.getLines) {
      if (!l.startsWith("#") && l.size != 0) {
        val keyValue = l.split(":")
        assert(keyValue.size == 2) // there should only one ":" in the string
        val key = keyValue(0).trim
        val value = keyValue(1).trim
        processValue(key, value, keysDict)
      }
      else if (l.size == 0) {
        contexts += new Context(collection.immutable.Map(keysDict.toSeq: _*))
        keysDict.clear()
      }
    }

    contexts
  }
}

/**
 * Created by smcho on 6/14/14.
 */
class Context(val map: collection.immutable.Map[String, Any]) {

  def this(map: collection.mutable.Map[String, Any]) =
    this(collection.immutable.Map[String, Any](map.toSeq: _*))

  def getObjectSize(value: Any) : Int = {
    if (value.isInstanceOf[String])
      value.asInstanceOf[String].size
    else if (value.isInstanceOf[Int])
      4
    else if (value.isInstanceOf[Float])
      4
    else if (value.isInstanceOf[Double])
      8
    else
      throw new Exception("Unknown type of object")
  }
  def size() = {
    var keySize = 0
    var valueSize = 0
    for ((key, value) <- map) {
      keySize += key.size
      valueSize += getObjectSize(value)
    }
    (keySize + valueSize, keySize, valueSize)
  }
  def print() = {
    println("<-----------------------------------------------")
    for ((key, value) <- map) {
      println(s"KEY($key) => ${value}")
    }
    println("----------------------------------------------->")
  }
  def keys() = {
    map.keys
  }
  def sizeMap() = map.size
}
