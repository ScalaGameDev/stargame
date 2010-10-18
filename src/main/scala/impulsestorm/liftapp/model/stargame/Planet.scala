package impulsestorm.liftapp.model.stargame

import impulsestorm.liftapp.lib._

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

case class Colony( starId: Int, settlements: List[Settlement] )

object Colony {
  def startingColony( star: Star ) = {
    val startingPlanet = SimRandom.randomObj(
      star.planets.filter(_.pType == PlanetType.Terran)
    )
      
    Colony(star.id, List(Settlement(startingPlanet.id, 50, 50))) 
  }
}

case class Settlement( planetId: Int, population: Double, capital: Double )                   

