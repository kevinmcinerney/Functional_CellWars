name := """play-scala"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.7"

// https://mvnrepository.com/artifact/org.scalanlp/breeze_2.11
libraryDependencies += "org.scalanlp" % "breeze_2.11" % "0.12"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.1" % Test
)



fork in run := true