package impulsestorm.stargame.model

import impulsestorm.stargame.lib.SimRandom

trait Fleet extends hasPosition { 
  val uuid: String
  val playerId: Int
  val ships: Int
  val fromStarId: Int
  
  def euid = "fv-" + uuid
  
  def coords(star: Star) = (star.x, star.y)
}

case class StationaryFleet( uuid: String, playerId: Int, 
                            ships: Int, fromStarId: Int )
extends Fleet
{
  def position(s: StarGameState) = coords(s.stars(fromStarId))
  
  def copyMoving(ships: Int, toStarId: Int, 
                 departYear: Double, arriveYear: Double) =
    MovingFleet(uuid, playerId, ships, fromStarId, toStarId, 
                departYear, arriveYear).newUUID()
  
  def newUUID() = copy(uuid=java.util.UUID.randomUUID().toString())
  
  def weaken(pSurviving: Double) = 
    copy(ships=math.max((pSurviving*ships).toInt, 1)) // at least one survivor
}
 
case class MovingFleet( uuid: String, playerId: Int,
                        ships: Int, 
                        fromStarId: Int, toStarId: Int,
                        departYear: Double, 
                        arriveYear: Double)
extends Fleet {
  
  def position(s: StarGameState) = {
    // linear interpolation
    val alpha = 1-(s.gameYear-departYear)/(arriveYear-departYear)
    val (xFrom, yFrom) = coords(s.stars(fromStarId))
    val (xTo, yTo)     = coords(s.stars(toStarId))
    
    (alpha*xFrom+(1-alpha)*xTo, alpha*yFrom+(1-alpha)*yTo)
  }
  
  def arrived(year: Double) : Boolean = year > arriveYear
  
  def arrive = StationaryFleet(uuid, playerId, ships, toStarId)
  
  def newUUID() = copy(uuid=java.util.UUID.randomUUID().toString())
}

object Fleet {
  
  def newFleet(playerId: Int, ships: Int, starId: Int) =
    StationaryFleet("", playerId, ships, starId).newUUID() 
  
  def startingFleet(playerId: Int, homeStarId: Int) =
    newFleet(playerId, 40, homeStarId)
  
  def mergeByPlayer(fleets: List[StationaryFleet]) = {
    // Believe a mutable map is the easiest way to do it, (unfortunately)
    val mergingMap = new collection.mutable.HashMap[Int, StationaryFleet]
   
    fleets.foreach(f =>
      if(mergingMap.contains(f.playerId)) {
        val existingFleet = mergingMap(f.playerId)
        val combinedFleet =   
          existingFleet.copy(ships=existingFleet.ships+f.ships)
        
        mergingMap.update(f.playerId, combinedFleet)
      } else {
        // add to map, as this is the first fleet of this player
        mergingMap.put(f.playerId, f)
      }
    )
    
    mergingMap.values.toSet
  }
  
  // guaranteed that fleets has length of at least one
  def doBattle(fleets: List[StationaryFleet], 
               players: List[Player]) : Option[StationaryFleet] = 
  {
    fleets.length match {
      case 0 => None
      case 1 => Some(fleets.head)
      case _ => {
        val scoresAndFleets = fleets.map( f => {
          val player = players(f.playerId)
          ((f.ships*player.battlePower*SimRandom.random(0.8, 1.2)), f)
        }).sortBy(_._1).reverse // sort by score
        
        Some(scoresAndFleets.head._2.weaken(
          1.0-scoresAndFleets(1)._1/scoresAndFleets(0)._1)) // score_2/score_1 
      }
    }
  }

}


