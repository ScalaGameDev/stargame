package impulsestorm.stargame.model.stargame

import impulsestorm.stargame.lib._

import scala.util.Random


case class Planet( id: Int, pType: PlanetType, 
                   zone: PlanetZone, baseMaxPop: Double,
                   mineralWealth: Double )

object Planet {
  def genRandom( id: Int, sClass: StarClass, zone: PlanetZone ) = {
    val pType = PlanetZone.randomPType(sClass, zone)
    Planet(id, pType, zone, 
           PlanetType.randomBaseMaxPop(pType),
           PlanetType.randomMineralWealth(pType)
          )
  }
}

case class Colony( starId: Int, ownerId: Int, settlements: List[Settlement],
  x: Double, y: Double)
  extends hasPositionSimple

object Colony {
  def startingColony( star: Star, ownerId: Int ) = {
    val startingPlanet = SimRandom.randomObj(
      star.planets.filter(_.pType == PlanetType.Terran)
    )
      
    Colony(star.id, ownerId, List(Settlement(startingPlanet.id, 50, 50)),
           star.x, star.y) 
  }
}

case class Settlement( planetId: Int, population: Double, capital: Double )                   

