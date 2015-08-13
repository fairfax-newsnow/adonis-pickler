import CommonUtils._

organization := "au.com.fairfax"

name := "adonis-pickler"

val projectVersion = "0.0.1"

version := projectVersion

val scalaV = "2.11.6"

val playV = "2.3.9"

val scalaTestV = "2.1.7"

scalaVersion := scalaV

(fork in Test) := false

libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaV,
    "com.typesafe.play" %% "play-json" % playV,
    "org.scalatest" %% "scalatest" % scalaTestV % "test"
)

publishMavenStyle := true

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>https://github.com/fairfax-newsnow/adonis-pickler</url>
    <licenses>
      <license>
        <name>The MIT License</name>
        <url>https://github.com/fairfax-newsnow/adonis-pickler/blob/master/LICENSE</url>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:fairfax-newsnow/adonis-pickler.git</url>
      <connection>scm:git:git@github.com:fairfax-newsnow/adonis-pickler.git</connection>
    </scm>)

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))


lazy val writeVersion = inputKey[Unit]("Write Version in File'")

writeVersion := versionWriter(() => Def.spaceDelimited("filename").parsed)(projectVersion)


import org.scoverage.coveralls.Imports.CoverallsKeys._

coverallsTokenFile := Option(s"""${Path.userHome.absolutePath}/.coveralls-credentials""")


val repoLocation = "fairfax-newsnow/adonis-pickler"

/* GitHub Release { */
GithubRelease.repo := repoLocation

GithubRelease.tag := s"v$projectVersion"

GithubRelease.releaseName := GithubRelease.tag.value

GithubRelease.commitish := "release"

GithubRelease.notesFile := GithubRelease.notesDir.value / s"${projectVersion}.md"

GithubRelease.releaseAssets := {

  val binNames = listFiles(target.value / "ci", "*.jar")

  println(s"fileNames: $binNames")

  binNames
}
/* } GitHub Release */
