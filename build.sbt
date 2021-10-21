name := "flumen"

version := "0.1"

scalaVersion := "2.12.14"

libraryDependencies ++= Seq(
  "org.typelevel"                 %% "cats-core"             % "2.6.1",
  "io.serverlessworkflow"         % "serverlessworkflow-api" % "2.0.0.Final",
  "io.swagger.parser.v3"          % "swagger-parser"         % "2.0.27",
  "com.softwaremill.sttp.client3" %% "core"                  % "3.3.14",
  "com.softwaremill.sttp.client3" %% "okhttp-backend"        % "3.3.14",
  "com.softwaremill.sttp.client3" %% "circe"                 % "3.3.14",
  "org.slf4j"                     % "slf4j-api"              % "1.7.32",
  "com.chuusai"                   %% "shapeless"             % "2.3.7"
) ++ Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % "0.14.1")

libraryDependencies ++= (Seq(
  "org.http4s" %% "http4s-dsl",
  "org.http4s" %% "http4s-blaze-server",
  "org.http4s" %% "http4s-circe"
).map(_                      % "0.23.3") ++ Seq(
  "org.scalatest"            %% "scalatest" % "3.1.1",
  "org.typelevel"            %% "log4cats-slf4j" % "2.1.1",
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.14.1"
)).map(_ % Test)

scalacOptions += "-Ypartial-unification"

addCompilerPlugin("org.typelevel" % "kind-projector"      % "0.13.2" cross CrossVersion.full)
addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.1")
