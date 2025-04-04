ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.4"

val projectName = "scala-parser-generator"


lazy val root = (project in file("."))
  .settings(
    name := projectName,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.3.0-SNAP4"
  )
