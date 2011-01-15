package impulsestorm.stargame.model

import impulsestorm.stargame.lib._

import scala.util.Random

// netMigration and naturalGrowth and for the last tick only
case class Planet( id: Int, pType: PlanetType, 
                   zone: PlanetZone, baseMaxPop: Double,
                   mineralWealth: Double, 
                   pop: Double, 
                   netMigration: Double,
                   naturalGrowth: Double ) 
{
  def wealthYield = pop*mineralWealth
}

object Planet {
  def genRandom( id: Int, sClass: StarClass, zone: PlanetZone ) = {
    val pType = PlanetZone.randomPType(sClass, zone)
    Planet( id, pType, zone, 
            PlanetType.randomBaseMaxPop(pType),
            PlanetType.randomMineralWealth(pType), 
            0.0, 0.0, 0.0 )
  }
}

