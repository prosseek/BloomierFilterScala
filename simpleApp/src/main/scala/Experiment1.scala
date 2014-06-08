/**
 * Created by smcho on 6/8/14.
 */

import core.{SimpleDecoder, SimpleEncoder}
import immutable.BloomierFilter

object Experiment1 extends App {
  // https://www.google.com/maps/@30.297548,-97.7370279
  var m = Map[String,Any]("Name" -> "Sungmin Cho", "Latitude" -> 30.297548, "Altitude" -> -97.7370279)
  var b = new BloomierFilter(keysDict = m, m = m.size*2, k = 3, q = 8*11, enc = new SimpleEncoder(), dec = new SimpleDecoder())
  //b.setEncoderDecoder(encoder = new SimpleEncoder(), decoder = new SimpleDecoder())
  //b.createTable()
  println(b.get("Name"))
  println(b.get("Latitude"))
  println(b.get("Altitude"))

  m = Map[String,Any]("Latitude" -> 30.297548, "Altitude" -> -97.7370279)
  b = new BloomierFilter(keysDict = m, m = m.size*2, k = 3, q = 8*11, enc = new SimpleEncoder(), dec = new SimpleDecoder())
  //b.setEncoderDecoder(encoder = new SimpleEncoder(), decoder = new SimpleDecoder())
  //b.createTable()
  println(b.get("Name"))
  println(b.get("Latitude"))
  println(b.get("Altitude"))
}
