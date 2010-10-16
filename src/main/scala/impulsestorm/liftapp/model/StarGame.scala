package impulsestorm.liftapp.model

import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import net.liftweb.common.Logger

import impulsestorm.liftapp.lib._

import scala.util.Random

import se.scalablesolutions.akka.actor.{Actor}

case class Planet( id: Int, pType: PlanetType.Value, 
                   zone: PlanetZone.Value, baseMaxPop: Double,
                   mineralWealth: Double )

object Planet {
  def genRandom( id: Int, sClass: StarClass.Value, zone: PlanetZone.Value ) = {
    val pType = PlanetZone.randomPType(sClass, zone)
    Planet(id, pType, zone, 
           PlanetType.randomBaseMaxPop(pType),
           PlanetType.randomMineralWealth(pType)
          )
  }
}
                   
case class Player( openid: Option[String], alias: String,
                   traits: List[Trait.Value],
                   exploredStarIds: List[Int],
                   gold: Double,
                   techs: List[Tech.Value], 
                   researchAlloc: Map[TechCategory.Value, Double] )

object Player {
  def startingPlayer( openid: Option[String], alias: String,
                      traits: List[Trait.Value], startingStarId: Int) = {
    Player(openid, alias, traits, exploredStarIds = List(startingStarId),
           gold = 0, techs = Nil, 
           researchAlloc=TechCategory.defaultAllocation)
  }
}

case class Colony( starId: Int, settlements: List[Settlement] )

object Colony {
  def startingColony( star: Star ) = {
    val startingPlanet = SimRandom.randomObj(
      star.planets.filter(_.pType == PlanetType.Terran)
    )
      
    Colony(star.id, List(Settlement(startingPlanet.Id, 50, 50))) 
  }
}

case class Settlement( planetId: Int, population: Double, capital: Double )

// if not moving, will just use the fromStarId attr and the others are undef
case class Fleet( ships: Map[ShipDesign, Int], moving: Boolean, 
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

