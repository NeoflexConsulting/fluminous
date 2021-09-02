name := "flumen"

version := "0.1"

scalaVersion := "2.12.14"

libraryDependencies ++= Seq(
  "org.scalatest"         %% "scalatest"              % "3.1.1" % "test",
  "org.typelevel"         %% "cats-core"              % "2.6.1",
  "io.serverlessworkflow" % "serverlessworkflow-api" % "2.0.0.Final"
)
val circeVersion = "0.14.1"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

scalacOptions += "-Ypartial-unification"
