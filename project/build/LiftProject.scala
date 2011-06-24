import sbt._
import de.element34.sbteclipsify._


class LiftProject(info: ProjectInfo) 
  extends DefaultWebProject(info) 
  with AkkaProject with Eclipsify
  {
  
  val liftVersion = "2.1"

  val scalatoolsSnapshot = 
    "Scala Tools Snapshot" at "http://scala-tools.org/repo-snapshots/"

  // If you're using JRebel for Lift development, uncomment
  // this line
  // override def scanDirectories = Nil

  override def libraryDependencies = Set(
    "net.liftweb" %% "lift-webkit" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-mongodb" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-openid" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-json" % liftVersion % "compile->default",
    "org.mortbay.jetty" % "jetty" % "6.1.22" % "test->default",
    "junit" % "junit" % "4.5" % "test->default",
    "org.scala-tools.testing" %% "specs" % "1.6.5" % "test->default"
  ) ++ super.libraryDependencies
  
  override def ivyXML =
    <dependencies>
      <dependency org="org.slf4j" name="slf4j-api" rev="1.5.11" force="true">
      </dependency>
      <exclude module="logback-classic" />
      <exclude module="logback-core" />
    </dependencies>
}
