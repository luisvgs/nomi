ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

libraryDependencies +="com.lihaoyi" %% "fastparse" % "2.2.2"
lazy val root = (project in file("."))
  .settings(
    name := "Nomi"
  )
