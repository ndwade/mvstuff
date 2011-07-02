name := "mvtuff"

version := "0.1"

scalaVersion := "2.9.0"

// Add multiple dependencies
libraryDependencies ++= Seq(
	"junit" % "junit" % "4.8" % "test",
	"org.scalatest" % "scalatest_2.9.0" % "1.6.1",
	"org.scala-tools.testing" % "scalacheck_2.9.0-1" % "1.9",
	"org.clapper" %% "grizzled-scala" % "1.0.6"
)

