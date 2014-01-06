name := "cspom"

organization := "fr.univ-valenciennes.concrete"

version := "1.3-SNAPSHOT"

resolvers += "sonatype-releases" at "http://oss.sonatype.org/content/repositories/releases/"

resolvers += "Concrete repository" at "http://scand1sk.github.io/concrete/repository"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
	"junit" % "junit" % "4.11" % "test",
	"com.novocode" % "junit-interface" % "0.10" % "test" // For launching tests in SBT
	)

publishTo := Some(
	Resolver.file("Concrete local repository",
		new File(Path.userHome.absolutePath+"/concrete/repository")))

EclipseKeys.withSource := true