resolvers += Resolver.sonatypeRepo("snapshots")

// Re-enable when scalariform mess is solved, see
// https://github.com/ensime/ensime-server/issues/792
// addSbtPlugin("org.ensime" % "ensime-sbt" % "0.1.5")

addSbtPlugin("com.eed3si9n" % "sbt-sh" % "0.1.0")

addSbtPlugin("com.scalapenos" %% "sbt-prompt" % "0.2.1")

addSbtPlugin("com.orrsella" %% "sbt-stats" % "1.0.5")

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.1.6")
