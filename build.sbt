ThisBuild / scalaVersion  := "2.13.8"

val catsVersion = "2.8.0"
val catsEffectVersion = "3.3.14"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsEffectVersion
)