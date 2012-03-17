name := "mvtuff"

version := "0.1"

scalaVersion := "2.9.1"

// fork in run := true

// baseDirectory in run := file("/Users/ndw/dev/mvstuff/src/test/resources")

// Add multiple dependencies
libraryDependencies ++= Seq(
	"junit" % "junit" % "4.8" % "test",
	"org.scalatest" %% "scalatest" % "1.6.1" % "test",
	"org.scala-tools.testing" % "scalacheck_2.9.0-1" % "1.9" % "test" 
)
//,
//	"org.clapper" %% "grizzled-scala" % "1.0.6",
//	"org.clapper" %% "argot" % "0.3.1"

