name := "flumen"

version := "0.1"

scalaVersion := "2.12.14"

libraryDependencies ++= Seq(
  "com.chuusai"   %% "shapeless" % "2.3.7",
  "org.scalatest" %% "scalatest" % "3.1.1" % "test",
  "org.typelevel" %% "cats-core" % "2.6.1"
)
