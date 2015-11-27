resolvers += Resolver.sonatypeRepo("snapshots")

// Emacs backend for Scala
addSbtPlugin("org.ensime" % "ensime-sbt" % "latest.integration")

// Run shell commands directly from SBT shell
addSbtPlugin("com.eed3si9n" % "sbt-sh" % "0.1.0")

// Project statistics (LoC, no classes, etc)
addSbtPlugin("com.orrsella" %% "sbt-stats" % "latest.integration")

// Display updates for project dependencies
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.1.9")

// Clean ivy cache with respect to project deps.  Great naming, by the way :)
addSbtPlugin("com.eed3si9n" % "sbt-dirty-money" % "0.1.0")

// A nice plugin for a custom SBT prompt, but the default prompt plays better
// with SBT Mode in Emacs
// addSbtPlugin("com.scalapenos" %% "sbt-prompt" % "0.2.1")

// Dependency graphs for SBT projects
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "latest.integration")

// Poop, poop, SBT
resolvers += Resolver.url(
  "team-boris",
  url("http://dl.bintray.com/team-boris/sbt-plugins"))(
  Resolver.ivyStylePatterns
)

addSbtPlugin("de.teamboris" % "sbt-poop" % "0.1.1")
