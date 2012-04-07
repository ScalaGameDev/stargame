package impulsestorm.stargame.model

import impulsestorm.stargame.lib._

import scala.util.Random

import scala.math._

// netMigration and naturalGrowth and for the last tick only
case class Planet( id: Int, pType: PlanetType, 
                   zone: PlanetZone, baseMaxPop: Double,
                   mineralWealth: Double, 
                   pop: Double, 
                   popGrowthRate: Double ) 
{
  import Tech._
  import Planet.popEpsilon
  
  def wealthYield = pop*mineralWealth 
  
  def canColonize(pTechs: List[Tech]) = pType.requisiteTech match {
    case Some(requisiteTech) => pTechs.contains(requisiteTech)
    case None => true
  }
  
  def atMaxPop(maxPop: Double) = {
    abs(maxPop-pop) < popEpsilon
  }
  
  def grow(player: Player, timeSpan: Double, starId: Int) = {
    val pTechs = player.techs
    
    val maxPop = baseMaxPop*player.maxPopMultiplier
    
    val growthRate = if(canColonize(pTechs)) {
      if(atMaxPop(maxPop)) 0.0 else {
        val hostileMultiplier = if(pType.requisiteTech.isDefined) {
          if(pTechs.contains(HostilePop)) 1.0 else 0.5
        } else 1.0
        
        val techMultiplier = 
          if(pTechs.contains(Grow4)) 2.0 
          else if(pTechs.contains(Grow3)) 1.7
          else if(pTechs.contains(Grow2)) 1.4
          else if(pTechs.contains(Grow1)) 1.2
          else 1.0
        
        // 5% if there's no inhibiting factors
        val logisticGrowthRate = 0.05*pop*(1-pop/maxPop)
        
        val baseRate = 
          if(logisticGrowthRate < 0.0)
            min(-0.1, logisticGrowthRate)
          else
            max(0.5, logisticGrowthRate)
        
        baseRate*hostileMultiplier*techMultiplier
      }
    } else if(pop > Planet.popEpsilon) {
      println("pop %f > 0.0".format(pop))
      max(-1.0, -pop) // conquered a planet can't be colonized
    }
    else 0.0
    
    if(growthRate != 0.0) {
      /*println("Grew planet %d of star %d at rate %f".format(
        id, starId, growthRate))*/
      
      val growth = timeSpan*growthRate
      
      // if the distance between "growth" and maxpop is less than the delta,
      // just set it to maxpop. Prevents overshooting
      if(abs(maxPop-pop) < abs(growth))
        copy(pop = maxPop, popGrowthRate = 0.0)
      else
        copy(pop = pop+growth, popGrowthRate = growthRate)
    } else this
  }
  
}

object Planet {
  def genRandom( id: Int, sClass: StarClass, zone: PlanetZone ) = {
    val pType = PlanetZone.randomPType(sClass, zone)
    Planet( id, pType, zone, 
            PlanetType.randomBaseMaxPop(pType),
            PlanetType.randomMineralWealth(pType), 
            0.0, 0.0 )
  }
  
  def minGrowthRate = 0.1 // 0.1 million per year
  
  def popEpsilon = 0.001
}

