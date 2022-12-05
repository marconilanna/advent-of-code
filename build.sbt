val utf8 = java.nio.charset.StandardCharsets.UTF_8.toString

lazy val root = project.in(file(".")).settings(
  name := "aoc"
, description := "Advent of Code"
, startYear := Option(2022)
, licenses += "MIT " -> url("http://opensource.org/licenses/MIT")
, developers := List(
    Developer(
      id = "marconilanna"
    , name = "Marconi Lanna"
    , email = "@marconilanna"
    , url = url("http://github.com/marconilanna")
    )
  )
)

lazy val common = project.settings(commonSettings)

lazy val aoc2020 = project.in(file("2020")).settings(commonSettings).dependsOn(common)
lazy val aoc2021 = project.in(file("2021")).settings(commonSettings).dependsOn(common)
lazy val aoc2022 = project.in(file("2022")).settings(commonSettings).dependsOn(common)

val commonSettings = Seq(
  Compile / scalaSource := baseDirectory.value
, Compile / resourceDirectory := (Compile / scalaSource).value / "resources"
, scalaVersion := "3.2.1"
, scalacOptions ++= Seq(
    "-deprecation" // Emit warning and location for usages of deprecated APIs
  , "-encoding", utf8 // Specify character encoding used by source files
  , "-explain" // Explain errors in more detail
  , "-feature" // Emit warning and location for usages of features that should be imported explicitly
  , "-unchecked" // Enable additional warnings where generated code depends on assumptions
  , "-Wconf:any:error" // Change every warning into an error
  , "-Werror" // Fail the compilation if there are any warnings
  , "-Wunused:all" // Enable `unused` warnings
  )
)
