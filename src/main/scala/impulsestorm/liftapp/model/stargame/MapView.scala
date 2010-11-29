package impulsestorm.liftapp.model.stargame

case class StarView(id: Int, name: Option[String], sClass: StarClass,
                    x: Double, y: Double, planets: Option[List[Planet]],
                    knownColonyOwnerId: Option[Int])
                    
case class MapView(starViews: List[StarView])

object MapView {
  def from(s: StarGameState, player: Player) = {
    val starViews = s.stars.map( star => {
      val knownColonyOwnerId = s.colonies.find(_.starId == star.id) match {
        case Some(c) =>
          // if we've met the owner (including ourselves)
          if(player.metPlayerIds.contains(c.ownerId)) Some(c.ownerId) else None 
        case None => None
      }
      
      val name = if(player.exploredStarIds.contains(star.id) ||
                    knownColonyOwnerId.isDefined) Some(star.name) else None
      
      val planets = 
        if(player.exploredStarIds.contains(star.id)) 
          Some(star.planets) else None
                    
      StarView(star.id, name, star.sClass, star.x, star.y, planets,
               knownColonyOwnerId)
    })
    MapView(starViews)
  }
}
