package tools.dataReaders

import scala.io.Source
/**
 * Created by smcho on 6/5/14.
 *
 * 3039163	Sant Julià de Lòria	Sant Julia de Loria	San Julia,San Julià,Sant Julia de Loria,Sant Julià de Lòria,Sant-Zhulija-de-Lorija,sheng hu li ya-de luo li ya,Сант-Жулия-де-Лория,サン・ジュリア・デ・ロリア教区,圣胡利娅-德洛里亚,圣胡利娅－德洛里亚	42.46372	1.49129	P	PPLA	AD		06				8022		921	Europe/Andorra	2013-11-23
 */
object geoDataReader {
  def parseDouble(s: String) = try { if (s.toDouble > 0.0) true } catch { case _ : Throwable => false }
  def getItems(item:String) = {
    // separate the input with ","
    // http://stackoverflow.com/questions/24066423/identifying-the-space-in-commas-with-regular-expression/24067722#24067722
    val splitted = item.split(""",""")

    // get location info which is after the comma
    val locationInfo = splitted.last.split("""\s+""")
    val nameInfo = splitted(0).split("""\s+""")

    // get name which is the 2nd or third column data (when there is a space in the city name
    val name = if (nameInfo.size > 4) nameInfo(1) + " " + nameInfo(2) else nameInfo(1)

    // find the index of the column that can be parsed as double data
    val index = locationInfo.map(parseDouble).zipWithIndex.find {case (v,i) => v == true}.getOrElse((false,-1))._2

    // name, latitude, altitude
    if (index != -1)
      (name, locationInfo(index).toDouble, locationInfo(index+1).toDouble)
    else
      (name, -1.0, -1.0)
  }

  def main(args:Array[String]) = {
    val directoryLocation = "/Users/smcho/code/bloomierFilterScala/data/geonames"
    val fileName = "simple.txt"

    Source.fromFile(directoryLocation + "/" + fileName).getLines
    val data = for (l <- Source.fromFile(directoryLocation + "/" + fileName).getLines.toArray if l.size > 0)
      yield (getItems(l))
    print(data.mkString(":"))
  }
}
