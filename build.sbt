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
    "com.novocode" % "junit-interface" % "0.11" % "test"))

lazy val day1 = project.settings(standardSettings(1)).dependsOn(util)
lazy val day2 = project.settings(standardSettings(2)).dependsOn(util)

lazy val root = project.in(file(".")).aggregate(util, day1, day2)