name := "CSPOM"

version := "1.3-SNAPSHOT"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
	"org.mozilla" % "rhino" % "1.7R4",
	"junit" % "junit" % "4.10" % "test",
	"com.novocode" % "junit-interface" % "0.9" % "test->default"
	)

