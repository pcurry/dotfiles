resolvers += Resolver.sonatypeRepo("snapshots")

// Emacs backend for Scala
addSbtPlugin("org.ensime" % "ensime-sbt" % "0.1.7")

// Run shell commands directly from SBT shell
addSbtPlugin("com.eed3si9n" % "sbt-sh" % "0.1.0")

// Project statistics (LoC, no classes, etc)
addSbtPlugin("com.orrsella" %% "sbt-stats" % "1.0.5")

// Display updates for project dependencies
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.1.9")

// Clean ivy cache with respect to project deps.  Great naming, by the way :)
addSbtPlugin("com.eed3si9n" % "sbt-dirty-money" % "0.1.0")

// A nice plugin for a custom SBT prompt, but the default prompt plays better
// with SBT Mode in Emacs
// addSbtPlugin("com.scalapenos" %% "sbt-prompt" % "0.2.1")
