lazy val scalafix = (project in file("."))
  .aggregate(core, bench)
  .settings(inThisBuild(buildSettings))
  .settings(inThisBuild(compilerSettings))
  .settings(ideSettings)
  .settings(noPublishSettings)  

lazy val core = (project in file("core"))
  .settings(ideSettings)
  .settings(publishSettings) 
  .settings(
     libraryDependencies ++= Seq(
       "org.scalatest" %% "scalatest" % "3.0.5" % Test,
       "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
     )
   )

lazy val bench = (project in file("bench"))
  .enablePlugins(JmhPlugin)
  .dependsOn(core)
  .settings(ideSettings)
  .settings(noPublishSettings)

val buildSettings = Seq(
  organization := "it.unich.scalafix",
  scalaVersion := "2.12.8",
  crossScalaVersions := Seq("2.11.12", "2.12.8")
)

val compilerSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Xlint:_",
    "-Ywarn-dead-code",
    "-Ywarn-value-discard",
    "-Ywarn-numeric-widen",
  ) ++ (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((12, _))  => Seq("-Ywarn-unused:linted", "-Ywarn-extra-implicit")
    case _ => Nil
  })
)

val noPublishSettings = Seq(
  skip in publish := true
)

val publishSettings = Seq(
  moduleName := "scalafix",
  description := "A Scala library for solving fixpoint equations",
  licenses := Seq("GPL-3.0" -> url("https://opensource.org/licenses/GPL-3.0")),
  homepage := Some(url("https://github.com/jandom-devel/ScalaFix")),
  startYear := Some(2015),
  developers := List(
    Developer(
      "amato",
      "Gianluca Amato", "gianluca.amato.74@unich.it",
      url("http://www.sci.unich.it/~amato/")
    )
  ),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/jandom-devel/ScalaFix"),
      "scm:git:https://github.com/jandom-devel/ScalaFix.git",
      Some("scm:git:https://github.com/jandom-devel/ScalaFix.git")
    )
  ),
  publishTo := {
    if (isSnapshot.value)
      Some(Opts.resolver.sonatypeSnapshots)
    else
      Some(Opts.resolver.sonatypeStaging)
  }
)

val ideSettings = Seq(
  EclipseKeys.eclipseOutput := Some("target.eclipse"),
  ideOutputDirectory in Compile := Some(baseDirectory.value / "target/idea/classes"),
  ideOutputDirectory in Test := Some(baseDirectory.value / "target/idea/test-classes"),
)
