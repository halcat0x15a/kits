import com.typesafe.sbt.SbtGit.GitKeys._

lazy val commonSettings = Seq(
  organization := "org.halcat",
  version := "0.7.0-SNAPSHOT",
  scalaVersion := "2.12.0",
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
  ),
  scalacOptions ++= Seq("-feature", "-language:higherKinds", "-Ypartial-unification"),
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

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(unidocSettings: _*).
  settings(site.settings ++ ghpages.settings: _*).
  settings(
    publishArtifact := false,
    publish := {},
    publishLocal := {},
    site.addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), "latest/api"),
    gitRemoteRepo := "git@github.com:halcat0x15a/kits.git"
  ).
  aggregate(core, free, lens)

lazy val core = (project in file("core")).
  settings(commonSettings: _*).
  settings(name := "kits-core")

lazy val free = (project in file("free")).
  settings(commonSettings: _*).
  settings(name := "kits-free").
  dependsOn(core)

lazy val lens = (project in file("lens")).
  settings(commonSettings: _*).
  settings(
    name := "kits-lens",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )
