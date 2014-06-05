package utils.conversion

import org.scalatest._

/**
 * Created by smcho on 6/4/14.
 */
class TestLocation extends FunSuite {
  test ("Latitude test") {
    var l = new Latitude(38, 53, 23, 55)
    assert(l.toByteArray().mkString(":") == "38:-22:-21:6")
    assert(l.toBitSet().mkString(":") == "1:2:5:9:11:13:14:15:16:17:19:21:22:23:25:26")
    l = new Latitude(-38, 53, 23, 55)
    assert(l.toByteArray().mkString(":") == "-38:-22:-21:6")
  }
  test ("Altitude test") {
    var l = new Altitude(38, 53, 23, 55)
    assert(l.toByteArray().mkString(":") == "38:-11:117:3")
    assert(l.toBitSet().mkString(":") == "1:2:5:8:10:12:13:14:15:16:18:20:21:22:24:25")
    l = new Altitude(-38, 53, 23, 55)
    assert(l.toByteArray().mkString(":") == "-38:-11:117:3")
  }
  test ("Simple Location test") {
    var l = new Latitude(38, 53, 23, 55)
    var a = new Altitude(38, 53, 23, 55)

    val loc = new Location(l, a)
    assert(loc.toBitSet().mkString(":") == "3:4:6:10:12:13:16:20:22:24:25:26:27:28:30:32:33:34:36:37:39:40:43:46:48:50:51:52:53:54:56:58:59:60:62:63")
    assert(loc.toByteArray().mkString(":") == "88:52:81:95:-73:73:125:-35")

    val x = new Location(loc.toByteArray())
    assert(x.getA().toString == "(38,53,23,55)")
    assert(x.getL().toString == "(38,53,23,55)")
  }
}
