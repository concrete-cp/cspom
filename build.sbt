name := "cspom"

organization := "fr.uphf"

version := "3.0-SNAPSHOT"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "fr.univ-valenciennes" %% "bitvectors" % "2.2",
  "fr.univ-valenciennes" %% "mdd" % "1.8",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  "org.apache.commons" % "commons-compress" % "1.18",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.storm-enroute" %% "scalameter-core" % "0.17",
  "org.scalatest" %% "scalatest" % "3.0.7" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
  "org.tukaani" % "xz" % "1.8",
  "com.lihaoyi" %% "fastparse" % "2.1.2"
)

scalacOptions ++= Seq(  
  "-Xdisable-assertions"
  , "-deprecation"
  //	"-unchecked"
  , "-Xlint"
  , "-target:jvm-1.8"
  //
  //	"-feature"
  //	"-Yinline-warnings"
)

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

//wartremoverWarnings ++= Warts.all


publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-u", s"${(target in Test).value / "test-reports"}")

// EclipseKeys.withBundledScalaContainers := false

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
