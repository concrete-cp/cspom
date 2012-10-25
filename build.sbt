name := "cspom"

organization := "fr.univ-valenciennes.cspfj"

version := "1.3-SNAPSHOT"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
	"org.mozilla" % "rhino" % "1.7R4",
	"junit" % "junit" % "4.10" % "test"
	)

publishTo := Some("CSP4J repository" at "sftp://web.sourceforge.net/home/groups/c/cs/cspfj/htdocs/repository")