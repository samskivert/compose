lazy val root = (project in file(".")).
  settings(
    name := "c0",
    version := "0.1.0",

    scalaVersion := "2.12.4",

    scalacOptions += "-Ywarn-unused:params,-implicits",

    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "fastparse" % "1.0.0",
      "com.novocode" % "junit-interface" % "0.11" % "test"
    )
  )
