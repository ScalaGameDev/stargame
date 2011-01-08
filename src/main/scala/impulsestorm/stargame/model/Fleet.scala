package impulsestorm.stargame.model

// if not moving, will just use the fromStarId attr and the others are undef
// ships: [# of Design1, # of Design 2 etc.] 
case class Fleet( uuid: String, playerId: Int,
                  ships: Int, troops: Int,
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
  
  def newUUID = copy(uuid=java.util.UUID.randomUUID().toString())
}

object Fleet {
  
  def startingFleet(playerId: Int, homeStar: Star) = {
    Fleet("", playerId,
          40, 0, false,
          homeStar.id, None,
          None, None).newUUID
  }
}


