organization         := "com.performantdata"
organizationName     := "Performant Data LLC"
organizationHomepage := Some(url("http://github.com/performantdata"))

name        := "suffixtree"
version     := "0.1.0"
description := "Suffix tree implementation."
startYear   := Some(2008)
homepage    := Some(url("https://github.com/performantdata/suffixtree"))
developers  := Developer("performantdata", "Michael", "", url("https://github.com/performantdata/")) :: Nil
scmInfo := Some(ScmInfo(
  browseUrl = url("https://github.com/performantdata/suffixtree"),
  connection = "scm:git:https://github.com/performantdata/suffixtree.git"
))

scalaVersion := "2.12.15"
scalacOptions ++= Seq(
  "-deprecation",
  "-explaintypes",
  "-feature",
  "-unchecked",
  "-encoding", "UTF-8",
)
Test / fork := true
Test / scalacOptions ++= Seq(
  "-Xmx6g",
)
libraryDependencies ++= Seq(
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",

  "org.scalatest" %% "scalatest" % "3.2.9" % Test,
  "org.slf4j" % "slf4j-jdk14" % "1.7.36" % Test,
  "org.openjdk.jol" % "jol-core" % "0.3.2" % Test,
)

lazy val downloadTestData = taskKey[Unit]("Download the human chromosome 1, hg38 version, FASTA format.")
downloadTestData := {
  import scala.sys.process._

  val log = streams.value.log
  val f = (Test / resourceDirectory).value / "dm6.fa.gz"
  url("http://hgdownload.cse.ucsc.edu/goldenPath/dm6/bigZips/dm6.fa.gz") #> f ! log
}
