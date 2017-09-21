
name := "Meerkat"

organization := "org.meerkat"

version := "0.1.0"

scalaVersion := "2.12.2"

parallelExecution in Test := false

logBuffered in Test := false

unmanagedSourceDirectories in Compile += baseDirectory.value / "src" / "macros" / "scala"


libraryDependencies ++= Seq(
	"org.scalactic" %% "scalactic" % "3.0.1",
	"org.scalatest" %% "scalatest" % "3.0.1" % "test",
	"com.google.guava" % "guava-testlib" % "22.0",
	"junit" % "junit" % "4.11",
	"commons-io" % "commons-io" % "2.4",
	"org.bitbucket.inkytonik.dsinfo" %% "dsinfo" % "0.4.0",
	"org.scala-graph" % "graph-core_2.11" % "1.11.4",
	"org.json4s" %% "json4s-jackson" % "3.4.2"
)

// SBT Eclipse configuration
EclipseKeys.withSource := true

