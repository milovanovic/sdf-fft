import Tests._

lazy val sdf_fft = Project("sdf_fft", file("."))
  .dependsOn(rocketchip, `rocket-dsp-utils`)
  .settings(libraryDependencies ++= rocketLibDeps.value)
  .settings(commonSettings)

lazy val commonSettings = Seq(
  organization := "edu.berkeley.cs",
  version := "1.6",
  scalaVersion := "2.13.10",
  assembly / test := {},
  assembly / assemblyMergeStrategy := { _ match {
    case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
    case _ => MergeStrategy.first}},
  scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-Ymacro-annotations"), // fix hierarchy API
 // unmanagedBase := (chipyardRoot / unmanagedBase).value,
  allDependencies := {
    // drop specific maven dependencies in subprojects in favor of Chipyard's version
    val dropDeps = Seq(("edu.berkeley.cs", "rocketchip"))
    allDependencies.value.filterNot { dep =>
      dropDeps.contains((dep.organization, dep.name))
    }
  },
  exportJars := true,
  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.mavenLocal))

lazy val firesimAsLibrary = sys.env.get("FIRESIM_STANDALONE") == None
lazy val firesimDir = if (firesimAsLibrary) {
  file("sims/firesim/sim/")
} else {
  file("../../sim")
}

val rocketChipDir = file("generators/rocket-chip")

/**
  * It has been a struggle for us to override settings in subprojects.
  * An example would be adding a dependency to rocketchip on midas's targetutils library,
  * or replacing dsptools's maven dependency on chisel with the local chisel project.
  *
  * This function works around this by specifying the project's root at src/ and overriding
  * scalaSource and resourceDirectory.
  */
def freshProject(name: String, dir: File): Project = {
  Project(id = name, base = dir / "src")
    .settings(
      Compile / scalaSource := baseDirectory.value / "main" / "scala",
      Compile / resourceDirectory := baseDirectory.value / "main" / "resources"
    )
}

val chiselVersion = "3.5.6"

lazy val chiselSettings = Seq(
  libraryDependencies ++= Seq("edu.berkeley.cs" %% "chisel3" % chiselVersion,
  "org.apache.commons" % "commons-lang3" % "3.12.0",
  "org.apache.commons" % "commons-text" % "1.9"),
  addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full))

val chiselTestVersion = "2.5.1"
lazy val chiselTestSettings = Seq(libraryDependencies ++= Seq("edu.berkeley.cs" %% "chisel-iotesters" % chiselTestVersion))

// Subproject definitions begin

// -- Rocket Chip --

// Rocket-chip dependencies (subsumes making RC a RootProject)
lazy val hardfloat  = (project in rocketChipDir / "hardfloat")
  .settings(chiselSettings)
//  .dependsOn(midasTargetUtils)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.json4s" %% "json4s-jackson" % "3.6.6",
      "org.scalatest" %% "scalatest" % "3.2.0" % "test"
    )
  )

lazy val `rocket-macros` = (project in rocketChipDir / "macros")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.json4s" %% "json4s-jackson" % "3.6.6",
      "org.scalatest" %% "scalatest" % "3.2.0" % "test"
    )
  )

scalafmtOnCompile := true

lazy val rocketchip = freshProject("rocketchip", rocketChipDir)
  .dependsOn(hardfloat, `rocket-macros`, cde)
  .settings(commonSettings)
  .settings(chiselSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.json4s" %% "json4s-jackson" % "3.6.6",
      "org.scalatest" %% "scalatest" % "3.2.0" % "test"
    )
  )
  .settings( // Settings for scalafix
  //  semanticdbEnabled := true,
  //  semanticdbVersion := scalafixSemanticdb.revision,
  //  scalacOptions += "-Ywarn-unused"
  )

lazy val rocketLibDeps = (rocketchip / Keys.libraryDependencies)
lazy val midasTargetUtils = ProjectRef(firesimDir, "targetutils")

lazy val dsptools = freshProject("dsptools", file("./tools/dsptools"))
  .settings(
    chiselSettings,
    chiselTestSettings,
    commonSettings,
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.+" % "test",
      "org.typelevel" %% "spire" % "0.17.0",
      "org.scalanlp" %% "breeze" % "1.1",
      "junit" % "junit" % "4.13" % "test",
      "org.scalacheck" %% "scalacheck" % "1.14.3" % "test",
  ))

lazy val `rocket-dsp-utils` = freshProject("rocket-dsp-utils", file("./tools/rocket-dsp-utils"))
  .dependsOn(rocketchip, cde, dsptools)
  .settings(libraryDependencies ++= rocketLibDeps.value)
  .settings(commonSettings)

lazy val cde = (project in file("tools/cde"))
  .settings(commonSettings)
  .settings(Compile / scalaSource := baseDirectory.value / "cde/src/chipsalliance/rocketchip")
