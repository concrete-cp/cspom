name := "cspom"

organization := "fr.univ-valenciennes.cspfj"

version := "1.3-SNAPSHOT"

resolvers += "sonatype-releases" at "http://oss.sonatype.org/content/repositories/releases/"

resolvers += "Concrete repository" at "http://cspfj.sourceforge.net/repository"


scalaVersion := "2.10.1"

libraryDependencies ++= Seq(
	"org.mozilla" % "rhino" % "1.7R4",
	"junit" % "junit" % "4.10" % "test",
	"org.jcp" % "jsr331" % "1.1.1"
	)

publishTo := Some(
	Resolver.sftp("Concrete publish repository",
		"web.sourceforge.net",
		"/home/groups/c/cs/cspfj/htdocs/repository"))
