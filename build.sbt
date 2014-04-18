name := "cspom"

organization := "fr.univ-valenciennes.concrete"

version := "2.0"

resolvers += "typesafe-relases" at "http://repo.typesafe.com/typesafe/releases"

resolvers += "Concrete repository" at "http://concrete-cp.github.io/concrete/repository"

scalaVersion := "2.11.0"

libraryDependencies ++= Seq(
	"org.kohsuke" % "bzip2" % "1.0",
	"junit" % "junit" % "4.11" % "test",
	"ch.qos.logback" % "logback-classic" % "1.1.1",
	"com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.0.3",
	"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
	"org.scala-lang.modules" %% "scala-xml" % "1.0.1",
	"com.novocode" % "junit-interface" % "0.10" % "test" // For launching tests in SBT
	)

publishTo := Some(
	Resolver.file("Concrete local repository",
		new File(Path.userHome.absolutePath+"/concrete/repository")))

EclipseKeys.withSource := true

org.scalastyle.sbt.ScalastylePlugin.Settings