
name := "Meerkat"

organization := "org.meerkat"

version := "0.1.0"

scalaVersion := "2.12.3"

parallelExecution in Test := false

logBuffered in Test := false

unmanagedSourceDirectories in Compile += baseDirectory.value / "src" / "macros" / "scala"


libraryDependencies ++= Seq(
	"org.scalactic" %% "scalactic" % "3.0.1",
	"org.scalatest" %% "scalatest" % "3.0.1" % Test,
	"com.google.guava" % "guava-testlib" % "23.0",
	"commons-io" % "commons-io" % "2.4",
	"org.bitbucket.inkytonik.dsinfo" %% "dsinfo" % "0.4.0",
	"org.scala-graph" %% "graph-core" % "1.12.0",
	"org.json4s" %% "json4s-jackson" % "3.4.2"
)

// SBT Eclipse configuration
EclipseKeys.withSource := true

