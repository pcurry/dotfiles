libraryDependencies += "com.lihaoyi" % "ammonite-repl" % "0.4.7" % "test" cross CrossVersion.full

initialCommands in (Test, console) := """ammonite.repl.Repl.run("")"""

// Configure sbt-dependency graph
net.virtualvoid.sbt.graph.Plugin.graphSettings
