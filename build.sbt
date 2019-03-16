import com.typesafe.sbt.SbtGit.GitKeys._

lazy val commonSettings = Seq(
  organization := "org.halcat",
  version := "0.9.0-SNAPSHOT",
  scalaVersion := "2.12.8",
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
  ),
  scalacOptions ++= Seq("-deprecation", "-feature", "-language:higherKinds", "-Ypartial-unification"),
  autoAPIMappings := true,
  publishMavenStyle := true,
  publishTo := Some(
    if (isSnapshot.value)
      Opts.resolver.sonatypeSnapshots
    else
      Opts.resolver.sonatypeStaging
  ),
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
  enablePlugins(ScalaUnidocPlugin, GhpagesPlugin).
  settings(
    publishArtifact := false,
    publish := {},
    publishLocal := {},
    siteSubdirName in ScalaUnidoc := "latest/api",
    addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), siteSubdirName in ScalaUnidoc),
    gitRemoteRepo := "git@github.com:halcat0x15a/kits.git"
  ).
  aggregate(core, free, eff)

lazy val core = (project in file("core")).
  settings(commonSettings: _*).
  settings(name := "kits-core")

lazy val free = (project in file("free")).
  settings(commonSettings: _*).
  settings(name := "kits-free").
  dependsOn(core)

lazy val eff = (project in file("eff")).
  settings(commonSettings: _*).
  settings(
    name := "kits-eff"
  )
