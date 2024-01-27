addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.11.0")

addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.5.12")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.2")

addSbtPlugin("com.github.tkawachi" % "sbt-doctest" % "0.10.0")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.3")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")

addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.15.0")

addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.4.17")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.9")

addSbtPlugin("org.typelevel" % "sbt-typelevel-mergify" % "0.6.5")

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.7")

libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % "0.11.1"
