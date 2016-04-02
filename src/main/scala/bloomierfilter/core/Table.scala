package bloomierfilter.core

import scala.collection.mutable
import scala.collection.mutable.{Map => MMap}
/**
  * Created by smcho on 4/1/16.
  */
class Table(val m:Int, val Q:Int) {
  //val table = Array.ofDim[Byte](m, Q)
  val table = MMap[Int, Array[Byte]]()

  def calculate_non_zero_n = {
    // we cannot use N because of CBF
    //m - table.count(p => p.forall(_ == 0))
    table.size
  }

//  def checkZeroElement(element: Seq[Byte]) : Boolean = {
//    element.forall(_ == 0)
//  }

  def checkAllZeroElementsInTable(neighbors: Seq[Int]) : Boolean = {
    neighbors.forall(!table.keySet.contains(_))
//    for (n <- neighbors) {
//      if (!checkZeroElement(table(n))) return false
//    }
//    true
  }

  /**
    * Gvien two byteArrays return the xor one by one
    *
    * @param a
    * @param j
    */
  def byteArrayXor(a:Array[Byte], j:Int) : Array[Byte] = {
    if (table.keySet.contains(j))
      byteArrayXor(a, table(j))
    else
      a
  }

  def byteArrayXor(a:Array[Byte], b:Array[Byte]) : Array[Byte] = {
    if (a.size != b.size) {
      throw new Exception(s"Array size is not the same: ${a.size} != ${b.size}")
    }
    val newArray = new Array[Byte](a.size)
    var k = 0
    for (ba <- a) {
      newArray(k) = (ba ^ b(k)).toByte
      k += 1
    }
    newArray
  }

  def apply(index:Int) = table(index)
  def update(index:Int, value:Array[Byte]) = table(index) = value


  def size = serialize.size

  def serialize = Array[Byte]()

  def getNonzeroLocation = {
    val bs = mutable.BitSet()
    table.keysIterator.foreach(bs.add(_))
  }

  def n = -1 // We do not know the size
}
