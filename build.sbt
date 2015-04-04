organization := "org.halcat"

version := "0.3.0"

scalaVersion := "2.11.6"

crossScalaVersions := Seq("2.10.5", "2.11.6")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"
)

scalacOptions ++= Seq("-feature", "-language:higherKinds")

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
