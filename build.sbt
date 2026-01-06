ThisBuild / scalaVersion := "3.3.7"
ThisBuild / version := "0.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "ScalaGroupProject",

    libraryDependencies ++= Seq(
      "org.openjfx" % "javafx-base" % "25.0.1",
      "org.openjfx" % "javafx-controls" % "25.0.1",
      "org.openjfx" % "javafx-graphics" % "25.0.1",
      "org.openjfx" % "javafx-fxml" % "25.0.1"
    )
  )

