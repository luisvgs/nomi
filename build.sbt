//> using scala "2"
//> using lib "com.lihaoyi::fastparse:2.2.2"
version := "0.1.0-SNAPSHOT"

scalaVersion := "2.13.8"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.2.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % "test"

lazy val root = (project in file("."))
  .settings(
    name := "Nomi"
  )
