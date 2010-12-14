package impulsestorm.liftapp.model.stargame

case class StarView(id: Int, name: Option[String], sClass: StarClass,
                    x: Double, y: Double, planets: Option[List[Planet]],
                    knownColonyOwnerId: Option[Int])

case class FleetView( playerId: Int, ships: Option[List[Int]], bulk: Int,
                      moving: Boolean,
                      fromStarId: Int, toStarId: Option[Int],
                      arriveClock: Option[Double], x: Double, y: Double )

object FleetView {
  import math._
  def calculateAll(s:StarGameState, player: Player) = {
    val playerColonies = s.colonies.filter(_.ownerId == player.id)
    
    val (playerFs, otherFs) = s.fleets.partition(_.playerId == player.id)
    
    val inRangeOtherFs = otherFs.filter( fleet =>
      playerColonies.exists( _.distanceTo(fleet) <= player.sensorRange))
    
    (playerFs union inRangeOtherFs).map(f => fromFleet(f, player.id))
  }
  
  def fromFleet(f: Fleet, playerId: Int) = {
    val ships = if(f.playerId == playerId) Some(f.ships) else None
    
    FleetView(f.playerId, ships, f.bulk, f.moving, 
              f.fromStarId, f.toStarId, f.arriveClock, f.x, f.y)
  }
}
                      
case class MapView(starViews: List[StarView], fleetViews: List[FleetView])

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
    
    val fleetViews = FleetView.calculateAll(s, player)
    
    MapView(starViews, fleetViews)
  }
}
