package immutable

/**
 * Created by smcho on 6/2/14.
 */
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

    var j:Int = 0
    for (n <- neighbors) {
      valueToRestore = utils.conversion.Utils.byteArrayXor(valueToRestore, table(n))
    }
    core.Decode.decode(key, valueToRestore, byteSize)
  }

  def create(keysDict:Map[String, Any], orderAndMatch:core.OrderAndMatch) = {

    var i:Int = 0
    for (key <- orderAndMatch.piList) {
      val value : Int = keysDict(key).asInstanceOf[Int]
      val neighbors = hasher.getNeighborhood(key, hashSeed)
      val mask = hasher.getM(key).toArray.map(_.toByte)
      val l = orderAndMatch.tauList(i)
      val L = neighbors(l) // L is the index in the table to store the value

      val encodedValue = core.Encode.encode(key, value, byteSize)
      var valueToStore = utils.conversion.Utils.byteArrayXor(mask, encodedValue)

      var j:Int = 0
      for (n <- neighbors) {
        if (j != l) {
          valueToStore = utils.conversion.Utils.byteArrayXor(valueToStore, table(n))
        }
      }
      table(L) = valueToStore

      i += 1
    }
  }
}
