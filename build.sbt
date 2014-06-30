name := "cspom"

organization := "fr.univ-valenciennes.concrete"

version := "2.2-SNAPSHOT"

// For BZip2
resolvers += "Concrete repository" at "http://concrete-cp.github.io/concrete/repository"

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
	"org.kohsuke" % "bzip2" % "1.0",
	"org.scalatest" %% "scalatest" % "2.2.0" % "test",
	"org.scalacheck" %% "scalacheck" % "1.11.4" % "test",
	"ch.qos.logback" % "logback-classic" % "1.1.2",
	"com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
	"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
	"org.scala-lang.modules" %% "scala-xml" % "1.0.2",
	"com.google.guava" % "guava" % "17.0",
	"com.github.axel22" %% "scalameter" % "0.5-M2"
	)

publishTo := Some(
	Resolver.file("Concrete local repository",
		new File(Path.userHome.absolutePath+"/concrete/repository")))

testOptions in Test <+= (target in Test) map {
  t => Tests.Argument(TestFrameworks.ScalaTest, "-u", s"${t / "test-reports"}")
}


//testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := false