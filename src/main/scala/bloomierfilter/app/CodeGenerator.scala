package bloomierfilter.app

import bloomierfilter.main.BloomierFilter

object CodeGenerator {

  def getTemplateString(template:String, replacement:Map[String, String]) = {
    replacement.foldLeft(template)((s:String, x:(String,String)) => ( "#\\{" + x._1 + "\\}" ).r.replaceAllIn( s, x._2 ))
  }

  def header = "int update(char* key, char* value, char* byteArray) {\n"
  def tail = "}\n"

  def template =
s"""
if (key == "#{key}") {
  for (int i = 0; i < #{size}; i++) {
      int location[] = #{locations};

      for (int j = 0; j < #{Q}; j++) {
          byteArray[#{start_location} + location[i] + j] = value[i * #{Q} + j];
      }
  }
}
"""

  def codeGen(information: Map[java.lang.String, Any]) = {

    def getContent(key:java.lang.String, value:List[Int], startLocation:Int, Q:Int) = {
      val replacement = Map("key" -> key, "size" -> value.length.toString,
        "locations" -> value.mkString("{",",","}"),
         "Q" -> Q.toString,
        "start_location" -> startLocation.toString)
      getTemplateString(template, replacement)
    }

    val keyToList = information("keyToList").asInstanceOf[Map[String, List[Int]]]
    val startLocation = information("startLocation").asInstanceOf[Int]
    val Q = information("Q").asInstanceOf[Int]

    val stringBuffer = new StringBuilder()
    stringBuffer ++= header
    keyToList foreach {
      case (key, value) => {
        stringBuffer ++= getContent(key, value.asInstanceOf[List[Int]], startLocation, Q)
      }
    }
    stringBuffer ++= tail

    stringBuffer.toString
  }
}
