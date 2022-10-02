val scala3Version = "3.0.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3-simple",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies += "org.typelevel" %% "cats-core"          % "2.7.0",
    libraryDependencies += "com.lihaoyi"   %% "upickle"            % "1.4.3",
    libraryDependencies += "com.lihaoyi"   %% "os-lib"             % "0.8.0",
    libraryDependencies += "org.ow2.sat4j" %  "org.ow2.sat4j.core" % "2.3.4"
  )