name := "spacer"

version := "0.1"

scalaVersion := "2.12.6"

enablePlugins(ScalaJSPlugin)

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.6"
)

// This is an application with a main method
scalaJSUseMainModuleInitializer := true