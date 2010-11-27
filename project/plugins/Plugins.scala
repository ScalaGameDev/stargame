import sbt._
 
class Plugins(info: ProjectInfo) extends PluginDefinition(info) {
  val akkaPlugin = "se.scalablesolutions.akka" % "akka-sbt-plugin" % "0.10"
  val eclipse = "de.element34" % "sbt-eclipsify" % "0.7.0"
}
