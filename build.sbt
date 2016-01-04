lazy val commonSettings = Seq(
  organization := "org.halcat",
  version := "0.6.0",
  scalaVersion := "2.11.7",
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.2.5" % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"
  ),
  scalacOptions ++= Seq("-feature", "-language:higherKinds"),
  target in Compile in doc := baseDirectory.value / "api",
  publishMavenStyle := true,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
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
    </developers>
  )
)

lazy val root = project in file(".") aggregate (core, free)

lazy val core = project in file("core") settings (commonSettings: _*) settings (name := "kits-core")

lazy val free = project in file("free") settings (commonSettings: _*) settings (name := "kits-free") dependsOn core
