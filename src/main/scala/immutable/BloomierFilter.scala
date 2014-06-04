package immutable

/**
 * Created by smcho on 6/2/14.
 */

object BloomierFilter {

  def checkZeroElement(element: Seq[Byte]) : Boolean = {
    element.forall(_ == 0)
  }

  //def checkAllZeroElementsInTable(neighbors: Seq[Int], table: Seq[Seq[Byte]]) : Boolean = {
  def checkAllZeroElementsInTable(neighbors: Seq[Int], table: Array[Array[Byte]]) : Boolean = {
    for (n <- neighbors) {
      if (!checkZeroElement(table(n))) return false
    }
    true
  }
}

class BloomierFilter(keysDict:Map[String, Any], m:Int, k:Int, q:Int, maxTry:Int = 5, initialHashSeed:Int = 0) {
  val hasher = core.BloomierHasher(m = m, k = k, q = q, hashSeed = initialHashSeed)
  val oamf = new core.OrderAndMatchFinder(keysDict = keysDict, m = m, k = k, q = q, maxTry = maxTry, initialHashSeed = initialHashSeed)
  val orderAndMatch = oamf.find()
  val byteSize = hasher.getByteSize(q)

  val table = Array.ofDim[Byte](m, byteSize)
  val hashSeed = orderAndMatch.hashSeed
  create(keysDict, orderAndMatch)

  def get(key: String) = {
    val neighbors = hasher.getNeighborhood(key, hashSeed)
    val mask = hasher.getM(key).toArray.map(_.toByte)
    var valueToRestore = mask

    if (BloomierFilter.checkAllZeroElementsInTable(neighbors, table)) {
      None
    } else {
      for (n <- neighbors) {
        valueToRestore = utils.conversion.Utils.byteArrayXor(valueToRestore, table(n))
      }
      core.Decode.decode(key, valueToRestore, byteSize)
    }
  }

  def create(keysDict:Map[String, Any], orderAndMatch:core.OrderAndMatch) = {

    for ((key, i) <- orderAndMatch.piList.zipWithIndex) {
      val value : Int = keysDict(key).asInstanceOf[Int]
      val neighbors = hasher.getNeighborhood(key, hashSeed)
      val mask = hasher.getM(key).toArray.map(_.toByte)
      val l = orderAndMatch.tauList(i)
      val L = neighbors(l) // L is the index in the table to store the value

      val encodedValue = core.Encode.encode(key, value, byteSize)
      var valueToStore = utils.conversion.Utils.byteArrayXor(mask, encodedValue)

      for ((n, j) <- neighbors.zipWithIndex) {
        if (j != l) {
          valueToStore = utils.conversion.Utils.byteArrayXor(valueToStore, table(n))
        }
      }
      table(L) = valueToStore
    }
  }
}
