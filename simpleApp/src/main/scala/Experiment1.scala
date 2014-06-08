/**
 * Created by smcho on 6/8/14.
 */
import immutable.BloomierFilter

object Experiment1 extends App {
  // https://www.google.com/maps/@30.297548,-97.7370279
  val m = Map[String,Any]("Name" -> "Sungmin Cho", "Latitude" -> 30.297548, "Altitude" -> -97.7370279)
  val b = new BloomierFilter(keysDict = m, m = m.size*2, k = 3, q = 8*11)
  println(b.get("Name"))
}
