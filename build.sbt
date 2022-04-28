val scala3Version = "3.1.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := ".",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
  )
