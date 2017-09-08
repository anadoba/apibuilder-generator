import play.sbt.PlayScala._

name := "apidoc-example-union-types-discriminator"

organization := "io.apibuilder.generator"

scalaVersion in ThisBuild := "2.12.3"

lazy val generated = project
  .in(file("generated"))
  .enablePlugins(PlayScala)
  .settings(
    libraryDependencies ++= Seq(
      ws,
      filters
    )
  )

lazy val api = project
  .in(file("api"))
  .dependsOn(generated)
  .aggregate(generated)
  .enablePlugins(PlayScala)
  .settings(
    routesImport += "io.apibuilder.example.union.types.discriminator.v0._",
    libraryDependencies ++= Seq(
      ws,
      filters,
      specs2 % Test,
      "org.scalatest" %% "scalatest" % "3.0.4" % Test,
      "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.1" % "test"
    )
  )
