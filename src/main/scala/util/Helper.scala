package util

import bloomierfilter.main.ByteArrayBloomierFilter

object Helper
{
  /**
    * Gvien two byteArrays return the xor one by one
    *
    * @param a
    * @param b
    */
  // TODO: if a or b is null, or all zero, return the other one
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

  /**
    * Check if input list has no duplication
    *
    * @param list
    * @tparam T
    */
  def noDuplication[T](list:Seq[T]) = {
    val size = list.size
    if (size == 0) true
    else {
      val set = list.toSet
      set.size == size
    }
  }

  def createMapWithUppercaseKeys(keysDictInput:Map[String, Array[Byte]]) = {
    val map = collection.mutable.Map[String, Array[Byte]]()
    for ((key,value) <- keysDictInput) {
      map(key.toUpperCase()) = value
    }

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

  def printContents(bf: ByteArrayBloomierFilter) : Unit = {
    printContents(bf, bf.keysDict.keys)
  }

  def printContents(bf: ByteArrayBloomierFilter, keys: Iterable[String]) : Unit = {
    println("BL---------------------------------------------")
    println(s"Size in bytes : ${bf.size}")
    for (key <- keys) {
      println(s"KEY($key) => ${bf.getByteArray(key).getOrElse(null)}")
    }
    println("---------------------------------------------BL")
  }
}
