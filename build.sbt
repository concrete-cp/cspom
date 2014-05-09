name := "cspom"

organization := "fr.univ-valenciennes.concrete"

version := "2.0.4-SNAPSHOT"

// For BZip2
resolvers += "Concrete repository" at "http://concrete-cp.github.io/concrete/repository"

scalaVersion := "2.11.0"

libraryDependencies ++= Seq(
	"org.kohsuke" % "bzip2" % "1.0",
	"org.scalatest" %% "scalatest" % "2.1.5" % "test",
	"org.scalacheck" %% "scalacheck" % "1.11.4" % "test",
	"ch.qos.logback" % "logback-classic" % "1.1.2",
	"com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
	"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
	"org.scala-lang.modules" %% "scala-xml" % "1.0.1"
	)

publishTo := Some(
	Resolver.file("Concrete local repository",
		new File(Path.userHome.absolutePath+"/concrete/repository")))

EclipseKeys.withSource := true

org.scalastyle.sbt.ScalastylePlugin.Settings

testOptions in Test <+= (target in Test) map {
  t => Tests.Argument(TestFrameworks.ScalaTest, "-u", s"${t / "test-reports"}")
}
