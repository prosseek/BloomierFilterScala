package bloomierfilter.util

import bloomierfilter.main.BloomierFilter

object Helper
{
  /**
    * Gvien two byteArrays return the xor one by one
    * @param a
    * @param b
    */
  def byteArrayXor(a:Array[Byte], b:Array[Byte]) = {
    if (a.size != b.size) {
      throw new Exception(s"Array size is not the same: ${a.size} != ${b.size}")
    }
    val newArray = new Array[Byte](a.size)
    var i = 0
    for (ba <- a) {
      newArray(i) = (ba ^ b(i)).toByte
      i += 1
    }
    newArray
  }

  def createMapWithUppercaseKeys(keysDictInput:Map[String, Any]) = {
    val map = collection.mutable.Map[String, Any]()
    for ((key,value) <- keysDictInput) {
      map(key.toUpperCase()) = value
    }

    // returns a non-mutable map
    collection.immutable.Map(map.toSeq: _*)
  }

  def checkZeroElement(element: Seq[Byte]) : Boolean = {
    element.forall(_ == 0)
  }

  def checkAllZeroElementsInTable(neighbors: Seq[Int], table: Array[Array[Byte]]) : Boolean = {
    for (n <- neighbors) {
      if (!checkZeroElement(table(n))) return false
    }
    true
  }

  def printContents(bf:BloomierFilter) : Unit = {
    printContents(bf, bf.keysDict.keys)
  }

  def printContents(bf:BloomierFilter, keys: Iterable[String]) : Unit = {
    println("BL---------------------------------------------")
    println(s"Size in bytes : ${bf.size}")
    for (key <- keys) {
      println(s"KEY($key) => ${bf.get(key).getOrElse(null)}")
    }
    println("---------------------------------------------BL")
  }
}
