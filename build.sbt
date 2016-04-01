name := "bloomierfilter"

version := "0.1"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"
target in Compile in doc := baseDirectory.value / "doc/api"

libraryDependencies += "chitchattype" %% "chitchattype" % "0.1"

// ScalaTest additional setup
// http://www.scalatest.org/install
//
// If you need to add classpath directory
// <http://stackoverflow.com/questions/23357490/add-directory-to-classpath-in-build-scala-of-sbt>
// (fullClasspath in Test) += Attributed.blank(file("./src/test/resources/util/file/"))