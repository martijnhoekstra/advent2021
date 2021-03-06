import sbt._
val scala3Version = "3.1.0"

lazy val util = project
  .in(file("util"))
  .settings(
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= List(
      "co.fs2" %% "fs2-core" % "3.2.2",
      "co.fs2" %% "fs2-io" % "3.2.2"
    )
  )

def standardSettings(i: Int) = List(
  name := s"day$i",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := scala3Version,
  scalacOptions ++= List("-rewrite", "-source:3.1", "-new-syntax"),
  libraryDependencies ++= List(
    "co.fs2" %% "fs2-core" % "3.2.2",
    "co.fs2" %% "fs2-io" % "3.2.2",
    "org.typelevel" %% "cats-parse" % "0.3.6",
    "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test,
    "org.typelevel" %% "munit-cats-effect-3" % "1.0.6" % Test
  )
)

lazy val day1 = project.settings(standardSettings(1)).dependsOn(util)
lazy val day2 = project.settings(standardSettings(2)).dependsOn(util)
lazy val day3 = project.settings(standardSettings(3)).dependsOn(util)
lazy val day4 = project.settings(standardSettings(4)).dependsOn(util)
lazy val day5 = project.settings(standardSettings(5)).dependsOn(util)
lazy val day6 = project.settings(standardSettings(6)).dependsOn(util)
lazy val day7 = project.settings(standardSettings(7)).dependsOn(util)
lazy val day8 = project.settings(standardSettings(8)).dependsOn(util)
lazy val day9 = project.settings(standardSettings(9)).dependsOn(util)
lazy val day10 = project.settings(standardSettings(10)).dependsOn(util)
lazy val day11 = project.settings(standardSettings(11)).dependsOn(util)
lazy val day12 = project.settings(standardSettings(12)).dependsOn(util)
lazy val day13 = project.settings(standardSettings(13)).dependsOn(util)
lazy val day14 = project.settings(standardSettings(14)).dependsOn(util)
lazy val day15 = project.settings(standardSettings(15)).dependsOn(util)
lazy val day16 = project.settings(standardSettings(16)).dependsOn(util)
lazy val day17 = project.settings(standardSettings(17)).dependsOn(util)
lazy val day18 = project.settings(standardSettings(18)).dependsOn(util)
lazy val day19 = project.settings(standardSettings(19)).dependsOn(util)
lazy val day20 = project.settings(standardSettings(20)).dependsOn(util)
lazy val day21 = project.settings(standardSettings(21)).dependsOn(util)
lazy val day22 = project.settings(standardSettings(22)).dependsOn(util)
lazy val day23 = project.settings(standardSettings(23)).dependsOn(util)
lazy val day24 = project.settings(standardSettings(24)).dependsOn(util)
lazy val day25 = project.settings(standardSettings(25)).dependsOn(util)

lazy val root = project
  .in(file("."))
  .aggregate(
    util,
    day1,
    day2,
    day3,
    day4,
    day5,
    day6,
    day7,
    day8,
    day9,
    day10,
    day11,
    day12,
    day13,
    day14,
    day15,
    day16,
    day17,
    day18,
    day19,
    day20,
    day21,
    day22,
    day23,
    day24,
    day25
  )
