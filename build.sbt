name := """Lilium_Project_Initialization_Bot"""
organization := "com.lilium"
version := "1.0.0"

resolvers ++= Seq(
  "Sonatype Releases" at "https://s01.oss.sonatype.org/content/repositories/releases",
  "Sonatype Snapshots" at "https://s01.oss.sonatype.org/content/repositories/snapshots",
  "ImageJ Public" at "https://maven.imagej.net/content/repositories/public/",
  "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases",
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

resolvers ++= Seq(
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "SonaType" at "https://oss.sonatype.org/content/groups/public",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Bintray" at "https://jcenter.bintray.com/"
)

val SigmaStateVersion = "5.0.5"
val ErgoContractsVersion = "1.0.0"
val ErgoAppKitVersion = "5.0.1"
val ScryptoVersion = "2.2.1"

val Ergo: List[ModuleID] = List(
//  "org.scorexfoundation" %% "scrypto" % ScryptoVersion,
  "org.ergoplatform" %% "ergo-appkit" % ErgoAppKitVersion
//  "org.scorexfoundation" %% "sigma-state" % SigmaStateVersion
)

//libraryDependencies += "com.lihaoyi" %% "requests" % "0.8.0"
libraryDependencies += "org.apache.httpcomponents" % "httpclient" % "4.5.14"

lazy val root = (project in file("."))
  .enablePlugins(PlayScala)
  .settings(
    libraryDependencies ++=
      Ergo
  )

val PlayCirceVersion = "2814.2"

scalaVersion := "2.12.15"

libraryDependencies += guice
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "5.1.0" % Test
libraryDependencies += "io.github.ergo-lend" % "edge_2.12" % "0.1-SNAPSHOT"
libraryDependencies += "io.github.getblok-io" % "getblok_plasma_2.12" % "1.0.1"
libraryDependencies += "com.dripower" %% "play-circe" % PlayCirceVersion
dependencyOverrides += "org.typelevel" %% "cats-kernel" % "0.9.0"

assembly / assemblyMergeStrategy := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x                             => MergeStrategy.first
}

assembly / assemblyJarName := s"${name.value}-${version.value}.jar"
