organization := "au.com.fairfax"

name := "adonis-pickler"

version := "0.0.1"

val scalaV = "2.11.6"

val playV = "2.3.9"

val scalaTestV = "2.1.7"

scalaVersion := scalaV

(fork in Test) := false

libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaV,
    "com.typesafe.play" %% "play-json" % playV % "test",
    "org.scalatest" %% "scalatest" % scalaTestV % "test"
)

