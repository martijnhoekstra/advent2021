import sbt._
val scala3Version = "3.1.0"

lazy val util = project.in(file("util")).settings(
  version := "0.1.0-SNAPSHOT",

  scalaVersion := scala3Version,
  libraryDependencies ++= List(
    "co.fs2" %% "fs2-core" % "3.2.2", 
    "co.fs2" %% "fs2-io" % "3.2.2",
  )
)

def standardSettings(i: Int) = List(name := s"day$i",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= List(
    "co.fs2" %% "fs2-core" % "3.2.2", 
    "co.fs2" %% "fs2-io" % "3.2.2",
    "org.typelevel" %% "cats-parse" % "0.3.6",
    "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test,
    "org.typelevel" %% "munit-cats-effect-3" % "1.0.6" % Test))

lazy val day1 = project.settings(standardSettings(1)).dependsOn(util)
lazy val day2 = project.settings(standardSettings(2)).dependsOn(util)
lazy val day3 = project.settings(standardSettings(3)).dependsOn(util)
lazy val day4 = project.settings(standardSettings(4)).dependsOn(util)
lazy val day5 = project.settings(standardSettings(5)).dependsOn(util)
lazy val day6 = project.settings(standardSettings(6)).dependsOn(util)

lazy val root = project.in(file(".")).aggregate(util, day1, day2, day3, day4, day5, day6)