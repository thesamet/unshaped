name := "unshaped"

version := "0.1"

crossScalaVersions in ThisBuild := Seq("2.10.7", "2.11.12", "2.12.4")

scalacOptions in ThisBuild ++= Seq(
  "-Xlog-reflective-calls"
)

lazy val macros = project.in(file("macros")).settings(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
  libraryDependencies += "com.google.protobuf" % "protobuf-java" % "3.5.1",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "macro-compat" % "1.1.1",
    "com.chuusai" %% "shapeless" % "2.3.2",
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch)
  ),

  // simulacrum
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.12.0",

  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
      case Some((2, scalaMajor)) if scalaMajor >= 11 => Seq()
      // in Scala 2.10, quasiquotes are provided by macro paradise
      case Some((2, 10)) => Seq()
//        Seq("org.scalamacros" %% "quasiquotes" % "2.1.1" cross CrossVersion.binary)
    }
  }


)

lazy val core = project.in(file("core")).dependsOn(macros).settings(
  libraryDependencies +=
    "org.scalatest" %% "scalatest" % "3.0.5" % "test",

  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.12.0",
)

lazy val benchmark = project.in(file("benchmark")).dependsOn(core).settings(
  PB.targets in Compile := Seq(
    scalapb.gen() -> (sourceManaged in Compile).value
  )
).enablePlugins(JmhPlugin)

lazy val root =
  project.in(file("."))
    .aggregate(macros, core)
