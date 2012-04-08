package impulsestorm.stargame.model

case class StarView( euid: String,
                     id: Int, name: Option[String], sClass: StarClass,
                     x: Double, y: Double, planets: Option[List[Planet]],
                     knownOwnerId: Option[Int],
                     factories: Option[Int],
                     queuedProduction: Option[Int],
                     visibleGarrison: Option[FleetView])
                     
case class FleetView( euid: String, uuid: String,
                      playerId: Int, ships: Int,
                      moving: Boolean,
                      fromStarId: Int, 
                      toStarId: Option[Int] = None,
                      arriveYear: Option[Double] = None, 
                      x: Double, y: Double )

object FleetView {
  import math._
  
  def inRangeItems[T <: hasPosition](state: StarGameState, sensorRange: Int, 
                                     sensors: Iterable[hasPosition], 
                                     objectsToDetect: Set[T]) : Set[T] = {
    
    objectsToDetect.filter( obj =>
      sensors.exists( _.distanceTo(state)(obj) < sensorRange ))
  }
  
  def visibleMovingFvs(s:StarGameState, player: Player) = {
    val playerStars = s.stars.filter(_.ownerIdOpt == Some(player.id))
    
    val (playerMovingFs, otherMovingFs) = 
      s.movingFleets.partition(_.playerId == player.id)
    
    val inRangeOtherFs = 
      inRangeItems(s, player.sensorRange, playerStars, otherMovingFs)
    
    (playerMovingFs | inRangeOtherFs).map(f => fromFleet(s, f))
  }
  
  def fromFleet(s: StarGameState, f: StationaryFleet) = {
    val (x,y) = f.position(s)
    FleetView(f.euid, f.uuid, f.playerId, f.ships, false,
              f.fromStarId, 
              None, None, x, y)
  }
  
  def fromFleet(s: StarGameState, f: MovingFleet) = {
    val (x,y) = f.position(s)
    FleetView(f.euid, f.uuid, f.playerId, f.ships, true,
              f.fromStarId, Some(f.toStarId), 
              Some(f.arriveYear), x, y)
  }
}
                      
case class MapBounds(xLeft: Double, xRight: Double, 
                     yTop: Double, yBottom: Double)

object MapBounds {
  def apply(s: StarGameState) : MapBounds = {
    val starXs = s.stars.map(_.x)
    val starYs = s.stars.map(_.y)
    MapBounds(starXs.min, starXs.max, starYs.min, starYs.max)
  }
}

case class MapView(starViews: List[StarView], movingFleetViews: Set[FleetView],
                   mapBounds: MapBounds, yearsPerDay: Double, 
                   playerInfo: PlayerInfo, gameYear: Double,
                   lastReports: List[BattleReport],
                   playerNames: List[String], gameStarted: Boolean)

object MapView {
  def from(s: StarGameState, player: Player) = {
    val starViews = s.stars.map( star => {
      def onlyIfExplored[T](arg: T) = 
        if(player.exploredStarIds.contains(star.id)) Some(arg) else None
      def onlyIfOwned[T](arg: T) = 
        if(star.ownerIdOpt == Some(player.id)) Some(arg) else None
      
      val knownOwnerId = star.ownerIdOpt match {
        case Some(ownerId) => onlyIfExplored(ownerId)
        case _ => None
      }
      
      val knownGarrison = 
        if(star.garrison.isDefined && star.ownerIdOpt == Some(player.id))
          Some(FleetView.fromFleet(s, star.garrison.get))
        else None
        
      StarView("sv-"+star.id,
               star.id, 
               onlyIfExplored(star.name), 
               star.sClass, star.x, star.y, 
               onlyIfOwned(star.planets),
               knownOwnerId,
               onlyIfOwned(star.factories),
               onlyIfOwned(star.queuedProduction),
               knownGarrison)
    })
    
    val latestPlayerReports = s.reports.filter(
      r => r.victorId == player.id || r.loserIds.contains(player.id)
    ).takeRight(10).reverse
    
    val movingFleetViews = FleetView.visibleMovingFvs(s, player)
    
    MapView(starViews, movingFleetViews, MapBounds(s), s.yearsPerDay, 
            PlayerInfo.from(player), s.gameYear, latestPlayerReports,
            s.players.map(_.alias), s.started)
  }
}
