name := "Coursera's Programming Languages course homework in Scala"

version := "1.0"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
    "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test" ,
    "org.specs2" % "specs2_2.10" % "1.13" % "test"
)

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                  "releases"  at "http://oss.sonatype.org/content/repositories/releases",
                  "Typesafe Snapshots" at "http://repo.typesafe.com/typesafe/snapshots/")