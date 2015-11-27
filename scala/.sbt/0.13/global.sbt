// Ammonite integration
libraryDependencies += "com.lihaoyi" % "ammonite-repl" % "0.4.8" % "test" cross CrossVersion.full
initialCommands in (Test, console) := """ammonite.repl.Repl.run("")"""

// Clear before build
triggeredMessage in ThisBuild := Watched.clearWhenTriggered

// Cancel executions with C-c
cancelable in Global := true
