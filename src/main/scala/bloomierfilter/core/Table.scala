package bloomierfilter.core

import scala.collection.{BitSet, mutable}
import scala.collection.mutable.{Map => MMap}
/**
  * Created by smcho on 4/1/16.
  */
class Table(val m:Int, val Q:Int) {
  //val table = Array.ofDim[Byte](m, Q)
  var table = MMap[Int, Array[Byte]]()
  // from m, we know the size of location header
  val M = util.conversion.Util.getBytesForBits(m)

  def calculate_non_zero_n = {
    // we cannot use N because of CBF
    //m - table.count(p => p.forall(_ == 0))
    table.size
  }

  def checkAllZeroElementsInTable(neighbors: Seq[Int]) : Boolean = {
    neighbors.forall(!table.keySet.contains(_))
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

  def getNonzeroLocations = {
    val bs = mutable.BitSet()
    table.keysIterator.foreach(bs.add(_))
  }

  def createTable(byteArray:Array[Byte]) = {

    table = MMap[Int, Array[Byte]]()

    def getSubByteArray(index:Int, byteArray:Array[Byte]) = {
      byteArray.slice(Q*index, Q*(1 + index))
    }

    val locationBits = byteArray.slice(0, M)
    val byteArrays = byteArray.slice(M, byteArray.size)

    val locationBitsBitSet = util.conversion.ByteArrayTool.byteArrayToBitSet(locationBits)
    locationBitsBitSet.toSeq.sorted.zipWithIndex.foreach {
      case (location, index) => {
        table(location) = getSubByteArray(index, byteArrays)
      }
    }
  }

  def serialize : Array[Byte] = {
    val locationBitsByteArray = util.conversion.ByteArrayTool.bitSetToByteArray(BitSet(table.keySet.toSeq:_*))
    (locationBitsByteArray /: table.keys.toSeq.sorted) ((acc, key) => acc ++ table(key))
  }

  def size = M + Q*table.size
}
