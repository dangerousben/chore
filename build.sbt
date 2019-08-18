val versions = new {
  val cats = "1.6.1"
  val catsEffect = "1.4.0"
  val scala = "2.12.8"
  val scalacheck = "1.13.5"
}

val chore = project.in(file("."))
  .settings(
    scalaVersion := versions.scala,
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % versions.cats,
      "org.typelevel" %% "cats-effect" % versions.catsEffect,
      "org.typelevel" %% "cats-effect-laws" % versions.catsEffect,
    ),
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % versions.scalacheck,
      "org.typelevel" %% "cats-laws" % versions.cats,
    ).map(_ % Test),
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "3")
  )
