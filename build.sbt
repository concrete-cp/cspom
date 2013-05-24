name := "cspom"

organization := "fr.univ-valenciennes.concrete"

version := "1.3-SNAPSHOT"

resolvers += "sonatype-releases" at "http://oss.sonatype.org/content/repositories/releases/"

resolvers += "Concrete repository" at "http://scand1sk.github.io/concrete/repository"


scalaVersion := "2.10.1"

libraryDependencies ++= Seq(
	"org.mozilla" % "rhino" % "1.7R4",
	"junit" % "junit" % "4.10" % "test",
	"org.jcp" % "jsr331" % "1.1.1"
	)

publishTo := Some(
	Resolver.file("Concrete local repository",
		new File(Path.userHome.absolutePath+"/concrete/repository")))
