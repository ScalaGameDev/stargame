package impulsestorm.liftapp.model

import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import net.liftweb.common.Logger

import impulsestorm.liftapp.lib._

import scala.util.Random

import se.scalablesolutions.akka.actor.{Actor}

case class Star( id: Int, name: String, sClass: StarClass.Value, 
                 x: Double, y: Double, planets: List[Planet])
                 
object Star {
  
  import StarClass._
  
  def getRandom( mapSizeL: Double, id: Int ) = {
    val sClass = SimRandom.weightedRandom(StarClass.abundances)
    val sName = "S"+id.toString
    
    
    Star(id, name=sName, sClass=sClass,
         x=Random.nextDouble*mapSizeL, y=Random.nextDouble*mapSizeL,
         planets = randomPlanets(sClass))
  }
  
  def randomPlanets( sClass: StarClass.Value ) : List[Planet] = {
    val nPlanets = StarClass.randomNPlanets(sClass)
    
    val zones = 
      List.fill(nPlanets)(StarClass.randomZone(sClass, nPlanets)).sorted
    
    val idZonePairs = (1 to nPlanets).toList zip zones 
    
    idZonePairs.map(p => Planet.genRandom(p._1, sClass, p._2))
  }
  
}

case class Planet( id: Int, pType: PlanetType.Value, 
                   zone: PlanetZone.Value )

object Planet {
  def genRandom( id: Int, sClass: StarClass.Value, zone: PlanetZone.Value ) = {
    val pType = PlanetZone.randomPType(sClass, zone)
    Planet(id, pType, zone)
  }
}
                   
case class Player( openid: Option[String], alias: String,
                   traits: List[Trait.Value],
                   exploredStarIds: List[Int],
                   gold: Double,
                   colonies: List[Colony],
                   fleetsStationary: List[FleetStationary],
                   fleetsMoving: List[FleetMoving],
                   techs: List[Tech.Value], 
                   researchAlloc: Map[TechCategory.Value, Double] )

case class Colony( starId: Int, settlements: List[Settlement] )

case class Settlement( planetId: Int, population: Double, capital: Double )

// no location: in transit
case class FleetStationary( ships: Map[ShipDesign, Int], locationStarId: Int )

case class FleetMoving( ships: Map[ShipDesign, Int], 
                        fromStarId: Int, toStarId: Int,
                        departClock: Double, arriveClock: Double )

case class ShipDesign( warpSpeed: Double )                        

object StarGame {  
  val supervisor = 
    Actor.actorOf(new StateSupervisor(StarGameMaster.spawn _)).start
}

object StarView {
  import impulsestorm.liftapp.lib.ImOpenIDVendor.loginFirst
                      
  val root = Menu("Stargame") / "stargame" / "index" submenus (
    Menu("New Stargame") / "stargame" / "new" >> loginFirst >> Hidden,
    Menu("Play Stargame") / "stargame" / "play" / ** >> loginFirst >> Hidden
  )

  val menus = List(root)  
    
  val rewrites = NamedPF[RewriteRequest, RewriteResponse]("StarGame") {
    case RewriteRequest(
          ParsePath("stargame" :: "play" :: gameId :: Nil, _, _, _), _, _) =>
      RewriteResponse(
        "stargame" :: "play" :: Nil, Map("gameId"->gameId))
  }
  
}

