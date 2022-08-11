ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "topl"
  )


val circeVersion = "0.14.1"
val scalaTestVersion = "3.2.13"

libraryDependencies ++= Seq(
  // circe
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,

  // test
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "org.scalatest" %% "scalatest-funsuite" % scalaTestVersion % "test"
)

mainClass in (Compile, run) := Some("com.topl.Main")
