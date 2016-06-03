name := "cspom"

organization := "fr.univ-valenciennes"

version := "2.8-SNAPSHOT"

scalaVersion := "2.11.8"

resolvers += "INGI Snapshots" at "http://artifactory.info.ucl.ac.be/artifactory/libs-snapshot-local/"


libraryDependencies ++= Seq(
	"com.typesafe.scala-logging" %% "scala-logging" % "3.4.0",
	"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
	"org.scala-lang.modules" %% "scala-xml" % "1.0.5",
	"org.apache.commons" % "commons-compress" % "1.11",
	"ch.qos.logback" % "logback-classic" % "1.1.7",
	"com.storm-enroute" %% "scalameter-core" % "0.7",
	"org.scalatest" %% "scalatest" % "2.2.6" % "test",
	"org.scalacheck" %% "scalacheck" % "1.12.5" % "test",
	"com.storm-enroute" %% "scalameter" % "0.7" % "test",
	"org.tukaani" % "xz" % "1.5"
	)

scalacOptions ++= Seq(
  "-optimise"
  , "-Xdisable-assertions"
  , "-deprecation" 
//	"-unchecked" 
  , "-optimise" 
  , "-Xlint" 
//	
//	"-feature"
//	"-Yinline-warnings"
)

//wartremoverWarnings ++= Warts.all

//javacOptions ++= Seq("-source", "1.7", "-target", "1.7")


publishTo :=  {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

testOptions in Test <+= (target in Test) map {
  t => Tests.Argument(TestFrameworks.ScalaTest, "-u", s"${t / "test-reports"}")
}


val scalaMeterFramework = new TestFramework("org.scalameter.ScalaMeterFramework")
 
testFrameworks += scalaMeterFramework 

parallelExecution in Test := false

testOptions += Tests.Argument(scalaMeterFramework, "-silent")

licenses := Seq("LGPL 3.0" -> url("https://www.gnu.org/licenses/lgpl-3.0.txt"))

homepage := Some(url("https://github.com/concrete-cp/cspom"))

publishMavenStyle := true

pomExtra in Global := {
  <scm>
    <connection>scm:git:github.com/concrete-cp/cspom.git</connection>
    <url>github.com/concrete-cp/cspom.git</url>
  </scm>

  <developers>
    <developer>
      <id>scand1sk</id>
      <name>Julien Vion</name>
      <url>http://vion.free.fr/perso</url>
    </developer>
  </developers>
}
