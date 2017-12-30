// Eliminating warning:
// [warn] SBT is using ivy to resolve dependencies which is known to be slow. Coursier is recommended: http://get-coursier.io
addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0")

// Fixing https://github.com/coursier/coursier/issues/450 during move to SBT 1.0
classpathTypes += "maven-plugin"
