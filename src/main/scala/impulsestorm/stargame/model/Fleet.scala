package impulsestorm.stargame.model

// if not moving, will just use the fromStarId attr and the others are undef
// ships: [# of Design1, # of Design 2 etc.] 
case class Fleet( uuid: String, playerId: Int,
                  ships: Int, 
                  moving: Boolean, 
                  fromStarId: Int, toStarId: Option[Int],
                  departYear: Option[Double], arriveYear: Option[Double]) 
extends hasPosition {
  
  def position(s: StarGameState) = {
    def coords(star: Star) = (star.x, star.y)
    
    if(moving) {
      // linear interpolation
      val alpha = 1-(s.gameYear-departYear.get)/(arriveYear.get-departYear.get)
      val (xFrom, yFrom) = coords(s.stars(fromStarId))
      val (xTo, yTo)     = coords(s.stars(toStarId.get))
      
      (alpha*xFrom+(1-alpha)*xTo, alpha*yFrom+(1-alpha)*yTo)
    } else coords(s.stars(fromStarId))
  }
  
  def updateIfArrived(year: Double) = 
    if(moving && year > arriveYear.get)
      copy(moving=false, fromStarId=toStarId.get, toStarId=None, 
           departYear=None, arriveYear=None)
    else this
  
  def playerStarKey = (playerId, fromStarId)
  
  def newUUID() = copy(uuid=java.util.UUID.randomUUID().toString())
}

object Fleet {
  
  def startingFleet(playerId: Int, homeStar: Star) = {
    Fleet("", playerId,
          40, false,
          homeStar.id, None,
          None, None).newUUID
  }
  
  def mergeStationaryFleets(fleets: Set[Fleet]) = {
    // merge stationary fleets of the same team
    val (movingFleets, stationaryFleets) = fleets.partition(_.moving)
 
    // Believe a mutable map is the easiest way to do it, (unfortunately)
    val mergingMap = new collection.mutable.HashMap[(Int, Int), Fleet]
   
    stationaryFleets.foreach(f =>
      if(mergingMap.contains(f.playerStarKey)) {
        val existingFleet = mergingMap(f.playerStarKey)
        val combinedFleet = 
          existingFleet.copy(ships=existingFleet.ships+f.ships)
        
        mergingMap.update(f.playerStarKey, combinedFleet)
      } else {
        // add to map, as this is the first fleet of this player/star combo
        mergingMap.put(f.playerStarKey, f)
      }
    )
    
    val mergedStationaryFleets = mergingMap.values.toSet
    
    movingFleets union mergedStationaryFleets
  }
  

}


