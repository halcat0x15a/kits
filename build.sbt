organization := "org.kittens"

version := "0.1.0"

scalaVersion := "2.11.6"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"

scalacOptions ++= Seq("-feature", "-language:higherKinds")
