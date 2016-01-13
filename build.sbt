// Project configuration

version := "0.9"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-deprecation", "-feature", "-Xlint", "-Xlint:-missing-interpolator")

libraryDependencies ++=  Seq(
  "org.scalatest" %% "scalatest" % "2.2.6" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
)

// Eclipse plugin

EclipseKeys.eclipseOutput := Some("target.eclipse")
