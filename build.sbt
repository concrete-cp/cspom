name := "cspom"

organization := "fr.univ-valenciennes.concrete"

version := "2.5"

// For BZip2
resolvers += "Concrete repository" at "http://concrete-cp.github.io/concrete/repository"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
	"org.kohsuke" % "bzip2" % "1.0",
	"org.scalatest" %% "scalatest" % "2.2.4" % "test",
	"org.scalacheck" %% "scalacheck" % "1.12.2" % "test",
	"ch.qos.logback" % "logback-classic" % "1.1.3",
	"com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
	"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
	"org.scala-lang.modules" %% "scala-xml" % "1.0.3",
	"com.storm-enroute" %% "scalameter" % "0.6" % "test"
	)

scalacOptions ++= Seq("-optimise"
	      , "-Xdisable-assertions"
	      , "-target:jvm-1.7"
//	"-deprecation", 
//	"-unchecked", 
//	"-optimise", 
//	"-Xlint", 
//	
//	"-feature",
//	"-Yinline-warnings"
)

//wartremoverWarnings ++= Warts.all

javacOptions ++= Seq("-source", "1.7", "-target", "1.7")


publishTo := Some(
	Resolver.file("Concrete local repository",
		new File(Path.userHome.absolutePath+"/concrete/repository")))

testOptions in Test <+= (target in Test) map {
  t => Tests.Argument(TestFrameworks.ScalaTest, "-u", s"${t / "test-reports"}")
}


testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := false

EclipseKeys.eclipseOutput in Compile := Some("target/scala-2.11/classes")
EclipseKeys.eclipseOutput in Test := Some("target/scala-2.11/test-classes")
