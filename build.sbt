import ReleaseTransformations._

ThisBuild / scalaVersion := "3.1.2"
ThisBuild / organization := "it.unich.scalafix"
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-source",
  "future",
  "-language:adhocExtensions",
  "-new-syntax"
)

lazy val scalafix = project
  .in(file("."))
  .aggregate(core, bench)
  .settings(noPublishSettings)
  .settings(
    Jmh / run := (bench / Jmh / run).evaluated
  )

lazy val core = project
  .settings(publishSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest-funspec" % "3.2.11" % Test,
      "org.scalatestplus" %% "scalacheck-1-15" % "3.2.11.0" % Test,
      "org.scalacheck" %% "scalacheck" % "1.15.4" % Test
    ),
    Compile / doc / scalacOptions ++= Seq("-doc-root-content", "rootdoc.txt"),
    name := "ScalaFix"
  )

lazy val bench = project
  .dependsOn(core)
  .enablePlugins(JmhPlugin)
  .settings(noPublishSettings)

val noPublishSettings = Seq(
  publish / skip := true
)

val publishSettings = Seq(
  moduleName := "scalafix",
  versionScheme := Some("early-semver"),
  description := "A Scala library for solving fixpoint equations",
  licenses := Seq("GPL-3.0" -> url("https://opensource.org/licenses/GPL-3.0")),
  homepage := Some(url("https://github.com/jandom-devel/ScalaFix")),
  startYear := Some(2015),
  developers := List(
    Developer(
      "amato",
      "Gianluca Amato",
      "gianluca.amato.74@gmail.com",
      url("http://www.sci.unich.it/~amato/")
    )
  ),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/jandom-devel/ScalaFix"),
      "scm:git:https://github.com/jandom-devel/ScalaFix.git",
      Some("scm:git:ssh://git@github.com/jandom-devel/ScalaFix.git")
    )
  ),
  publishTo := {
    if (isSnapshot.value)
      Some(Opts.resolver.sonatypeSnapshots)
    else
      sonatypePublishToBundle.value
  },
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    releaseStepCommandAndRemaining("publishSigned"),
    releaseStepCommand("sonatypeBundleRelease"),
    setNextVersion,
    commitNextVersion,
    pushChanges
  )
)
