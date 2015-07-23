resolvers += Resolver.sonatypeRepo("snapshots")

addSbtPlugin("org.ensime" % "ensime-sbt" % "0.1.7")

addSbtPlugin("com.eed3si9n" % "sbt-sh" % "0.1.0")

addSbtPlugin("com.orrsella" %% "sbt-stats" % "1.0.5")

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.1.6")

// A nice plugin for a custom SBT prompt, but the default prompt plays better
// with SBT Mode in Emacs
// addSbtPlugin("com.scalapenos" %% "sbt-prompt" % "0.2.1")
