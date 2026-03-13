ThisBuild / scalaVersion := "3.6.4"
ThisBuild / organization := "com.surd"

lazy val root = (project in file("."))
  .settings(
    name := "surd",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.18" % Test
    )
  )
