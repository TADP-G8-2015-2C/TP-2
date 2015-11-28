name := "TP-2"

version := "1.0"

scalaVersion := "2.11.7"

sbtVersion := "0.13.9"

//Dependencies
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

//coverage
ScoverageSbtPlugin.ScoverageKeys.coverageMinimum := 60

ScoverageSbtPlugin.ScoverageKeys.coverageFailOnMinimum := false

ScoverageSbtPlugin.ScoverageKeys.coverageHighlighting := {
  if (scalaBinaryVersion.value == "2.10") false
  else false
}
