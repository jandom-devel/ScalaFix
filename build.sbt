/**
 * Copyright 2015 - 2022 Gianluca Amato <gianluca.amato@unich.it> and
 *                       Francesca Scozzari <francesca.scozzari@unich.it>
 *
 * This file is part of ScalaFix. ScalaFix is free software: you can
 * redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * ScalaFix is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of a MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * ScalaFix. If not, see <http://www.gnu.org/licenses/>.
 */
 
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
  "-new-syntax"
)

lazy val scalafix = project
  .in(file("."))
  .aggregate(core, bench)
  .settings(noPublishSettings)

lazy val core = project
  .settings(scaladocSettings)
  .settings(publishSettings)
  .settings(
    name := "ScalaFix",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest-funspec" % "3.2.11" % Test,
      "org.scalatestplus" %% "scalacheck-1-15" % "3.2.11.0" % Test,
      "org.scalacheck" %% "scalacheck" % "1.15.4" % Test
    ),
    Test / scalacOptions ++= Seq("-language:adhocExtensions"),
    Compile / doc / scalacOptions ++= Seq(
      "-doc-root-content",
      "core/api.md",
      "-siteroot",
      "core/"
    )
  )

lazy val bench = project
  .dependsOn(core)
  .enablePlugins(JmhPlugin)
  .settings(scaladocSettings)
  .settings(noPublishSettings)
  .settings(
    name := "ScalaFix Benchmarks"
  )

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

val scaladocSettings = Seq(
  Compile / doc / scalacOptions ++= Seq(
    "-project-version",
    version.value,
    "-project-footer",
    "The Jandom Development Team &mdash; Università di Chieti-Pescara, Italy",
    "-social-links:github::https://github.com/jandom-devel/ScalaFix",
    "-source-links:github://jandom-devel/ScalaFix/" +
      (if (version.value.endsWith("-SNAPSHOT")) "master" else "v" + version.value),
    "-external-mappings:.*scala.*::scaladoc3::https://scala-lang.org/api/3.x/," +
      ".*java.*::javadoc::https://docs.oracle.com/en/java/javase/11/docs/api/java.base/"
  )
)
