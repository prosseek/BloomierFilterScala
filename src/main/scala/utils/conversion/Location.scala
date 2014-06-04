package utils.conversion

/**
 * Created by smcho on 6/2/14.
 */
object Location {
  def dms2dd(d:Int, m:Int, s:Double) = {
    d.toDouble + m.toDouble/60.0 + s/3600.0
  }
  def dms2dd(d:Int, m:Int, s:String) : Double = {
    dms2dd(d,m,s.toDouble)
  }
  def dms2dd(d:Int, m:Int, s1:Int, s2:Int) : Double = {
    val s = s"${s1}.${s2}"
    dms2dd(d,m,s.toDouble)
  }
  def dd2dms(data:Double) = {
    // http://en.wikipedia.org/wiki/Decimal_degrees
    val d = data.toInt
    val m = (math.abs(data)*60.0).toInt % 60
    val value = math.abs(data)*3600.0
    val s1 = value.toInt % 60 // 0.5 is added to get round value
    val s2 = ((value - value.toInt)*100 + 0.5).toInt
    (d, m, s1, s2)
  }
}

class Location() {
  var dd : Double = 0.0
  var d :Int = 0
  var m :Int = 0
  var s1 :Int = 0
  var s2 :Int = 0

  def this(d:Int, m:Int, s1:Int, s2:Int) = {
    this()
    this.d = d
    this.m = m
    this.s1 = s1
    this.s2 = s2
    this.dd = Location.dms2dd(d,m,s1,s2)
  }
  def this(dd:Double) = {
    this()
    this.dd = dd
    val (d, m, s1, s2) = Location.dd2dms(dd)
    this.d = d
    this.m = m
    this.s1 = s1
    this.s2 = s2
  }
  def toByteArray() = {
    //
  }
}
