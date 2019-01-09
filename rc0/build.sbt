val dottyVersion = "0.11.0-RC1"

lazy val root = project.in(file(".")).settings(
  name := "rc0",
  version := "0.1.0",
  scalaVersion := dottyVersion,
  scalacOptions += "-language:strictEquality",
  libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
)
