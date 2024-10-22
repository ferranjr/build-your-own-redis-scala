ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.15"

lazy val root = (project in file("."))
  .settings(
    name := "build-your-own-redis-scala",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test
  )
