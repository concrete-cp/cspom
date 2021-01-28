name := "cspom"

organization := "com.github.concrete-cp"

version := "3.2-SNAPSHOT"

scalaVersion := "2.13.1"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  "com.github.concrete-cp" %% "bitvectors" % "3.0",
  "com.github.concrete-cp" %% "mdd" % "2.1",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  "org.apache.commons" % "commons-compress" % "1.19",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
  "org.tukaani" % "xz" % "1.8",
  "com.lihaoyi" %% "fastparse" % "2.3.1"
)

scalacOptions ++= Seq(  
  "-Xdisable-assertions",
  "-deprecation",
  //	"-unchecked"
  "-Xlint",
//  "-target:jvm-1.8",
  //	"-feature"
  //	"-Yinline-warnings"
)

// javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

//wartremoverWarnings ++= Warts.all

publishArtifact in Test := false

// testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-u", s"${(target in Test).value / "test-reports"}")

parallelExecution in Test := false

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

publishTo := sonatypePublishToBundle.value
