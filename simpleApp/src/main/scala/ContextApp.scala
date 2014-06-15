/**
 * Created by smcho on 6/12/14.
 */

import immutable.BloomierFilter
import core.Context

object ContextApp extends App {

  val directory = "/Users/smcho/Desktop/grapevine_samsung_experiments/contexts/"
  val fileName = "context1.txt"

  val contexts = Context.readContextsFromFile(directory, fileName)
  val c = contexts(0)
  c.print()

  BloomierFilter.maxTry = 10
  var m = c.sizeMap * 4
  val byteSize = 5
  val k = 3

  BloomierFilter.caseSensitive = true
  var b = new BloomierFilter(c, m = m, k = k, q = 8 * byteSize)
  b.printContents()

  println(">>>")
  println(b.get("KEY VALUE").getOrElse(null))
  println(b.get("KEY VALUE I").getOrElse(null))
  println(b.get("KEY VALUE F").getOrElse(null))
  println(b.get("KEY VALUE S").getOrElse(null))
  println(b.get("TEMPERATURE").getOrElse(null))
  println(b.get("TIME").getOrElse(null))

  BloomierFilter.caseSensitive = false
  b = new BloomierFilter(c, m = m, k = k, q = 8 * byteSize)
  b.printContents()

  println(">>>")
  println(b.get("KEY VALUE2").getOrElse(null))
  println(b.get("KEY VALUE2 I").getOrElse(null))
  println(b.get("KEY VALUE2 F").getOrElse(null))
  println(b.get("KEY VALUE2 S").getOrElse(null))
  println(s"Latitude: ${b.get("LATITUDE").getOrElse(null)}")
  println(s"Altitude: ${b.get("ALTITUDE").getOrElse(null)}")
  println(s"Temperature: ${b.get("TEMPERATURE").getOrElse(null)}")
  println(s"Date: ${b.get("DATE").getOrElse(null)}")
  println(s"Time: ${b.get("TIME").getOrElse(null)}")
}
