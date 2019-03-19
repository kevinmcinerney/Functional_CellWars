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

// https://mvnrepository.com/artifact/com.google.code.gson/gson
libraryDependencies += "com.google.code.gson" % "gson" % "2.3.1"

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"
libraryDependencies += "com.storm-enroute" %% "scalameter-core" % "0.6"

resolvers += "bintray-spark-packages" at "https://dl.bintray.com/spark-packages/maven/"

val sparkVersion = "2.1.0"

libraryDependencies += "org.apache.spark" %% "spark-core" % sparkVersion
libraryDependencies += "org.apache.spark" %% "spark-sql" % sparkVersion
libraryDependencies += "graphframes" % "graphframes" % "0.7.0-spark2.4-s_2.11"
libraryDependencies += "org.apache.spark" %% "spark-graphx" % sparkVersion






