organization := "org.halcat"

version := "0.6.0-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.5" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.4" % "test",
  "org.scalaz" %% "scalaz-core" % "7.2.0"
)

scalacOptions ++= Seq("-feature", "-language:higherKinds")

fork in run := true

enablePlugins(JmhPlugin)

//javaOptions ++= Seq(/*"-Djava.compiler=none", */"-XX:+PrintCompilation")

target in Compile in doc := baseDirectory.value / "api"

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>https://github.com/halcat0x15a/kits</url>
  <licenses>
    <license>
      <name>BSD-style</name>
      <url>http://www.opensource.org/licenses/bsd-license.php</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:halcat0x15a/kits.git</url>
    <connection>scm:git:git@github.com:halcat0x15a/kits.git</connection>
  </scm>
  <developers>
    <developer>
      <id>halcat0x15a</id>
      <name>Sanshiro Yoshida</name>
      <url>http://halcat.org</url>
    </developer>
  </developers>)
