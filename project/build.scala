import sbt._
import Keys._

object build extends Build {
  lazy val root = Project(
    id = "root",
    base = file("."),
    aggregate = Seq(plugin, main)
  )

  lazy val sharedSettings = Seq(
    scalaVersion := "2.11.0-RC1",
    scalaHome := Some(file("/home/vova/scala-projects/GSoC/scala/scala/build/pack")),
    organization := "org.scala-lang.plugins",
    name         := "mars"
  )

  // This subproject contains Mars compiler plugin
  lazy val plugin = Project(
    id   = "plugin",
    base = file("plugin")
  ) settings (
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _),
    publishArtifact in Compile := false
  ) settings (sharedSettings : _*)

  // Scalac command line options to install our compiler plugin.
  lazy val usePluginSettings = Seq(
    scalacOptions in Compile <++= (Keys.`package` in (plugin, Compile)) map { (jar: File) =>
       val addPlugin = "-Xplugin:" + jar.getAbsolutePath
       // add plugin timestamp to compiler options to trigger recompile of
       // main after editing the plugin. (Otherwise a 'clean' is needed.)
       val dummy = "-Jdummy=" + jar.lastModified
       Seq(addPlugin, dummy)
    }
  )

  // A regular module with the application code (marsExplorer).
  lazy val main = Project(
    id   = "main",
    base = file("main")
  ) settings (sharedSettings ++ usePluginSettings: _*)
}
