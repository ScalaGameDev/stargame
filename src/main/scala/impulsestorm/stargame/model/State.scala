package impulsestorm.stargame.model

import impulsestorm.stargame.lib._
import net.liftweb.common.Logger

import org.bson.types.ObjectId

import scala.util.Random

import scala.annotation.tailrec

import java.util.Date

import _root_.net.liftweb.mongodb._

case class StarGameState( _id: String, createdBy: String, name: String, 
                          mapSize: String, nPlayers: Int,
                          
                          started: Boolean = false,
                          finished: Boolean = false,
                          gameVictor: Int = -1,
                          
                          realStartTime: Date,
                          lastMutateTime: Date,
                          
                          gameYear: Double = 0,
                          
                          yearsPerDay: Double,
                          
                          stars: List[Star], players: List[Player],
                          movingFleets: Set[MovingFleet], 
                          availableStartStarIds: List[Int],
                          reports: List[BattleReport] = Nil)
  extends State[StarGameState] with MongoDocument[StarGameState] {
  
  def addedPlayer(pSpec: PlayerSpec) = {
    val newPlayerId = players.length // same as array index
    val homeStarId = availableStartStarIds.head
    
    val newPlayers = 
      players :+ Player.startingPlayer(newPlayerId, pSpec, homeStarId)
    
    val homeStar = stars(homeStarId)
    val homePlanet = 
      SimRandom.randomObj(homeStar.planets.filter(_.pType == PlanetType.Terran))
      
    val newPlanets = 
      homeStar.planets.updated(homePlanet.id, homePlanet.copy(pop = 50))
      
    val newHomeStar = 
      homeStar.copy(ownerIdOpt=Some(newPlayerId), planets=newPlanets,
                    garrison=Some(Fleet.startingFleet(newPlayerId, homeStarId)))
      
    val newStars = stars.updated(homeStar.id, newHomeStar)
      
    this.copy(availableStartStarIds = availableStartStarIds.tail,
              players = newPlayers, stars = newStars)
  }
  
  def updatedPlayer(p: Player) =
    this.copy(players=players.updated(p.id, p))
  
  def supposedToBeYear = {
    val msSinceStart = (new Date()).getTime - realStartTime.getTime
    val daysSinceStart = msSinceStart/86400.0/1000.0
    daysSinceStart*yearsPerDay
  }
  
  def advancedOneTick() = {
    val curYear = gameYear+StarGameState.tickSizeYears
    
    // star growth
    val stars2 = stars.map(s => {
      if(s.ownerIdOpt.isDefined) {
        val owningPlayer = players(s.ownerIdOpt.get)
        val newPlanets = 
          s.planets.map(_.grow(owningPlayer, StarGameState.tickSizeYears, s.id))
        
        s.copy(planets = newPlanets)
      } else s
    })
    
    // newly arrived ships and moving fleets
    val (newlyArrivedFleets, newMovingFleets) = 
      movingFleets.partition( _.arrived(curYear) )
    
    val (stars3, newReports) = stars2.map(s => {
      // 1. process ship production
      val s1 = s.producedShips(StarGameState.tickSizeYears)
      
      // 2. calculate all the ships here including arrivals and merging
      val fleetsArrivedHere = 
        newlyArrivedFleets.filter(_.toStarId == s1.id).map(_.arrive).toList
      
      val allFleetsHere = 
        Fleet.mergeByPlayer(s1.garrison.toList ++ fleetsArrivedHere)
      
      // 3. do battle, leaving just one fleet standing
      val finalGarrison = Fleet.doBattle(allFleetsHere.toList, players)
      
      // 4. generate battle report
      val reportOpt : Option[BattleReport] = if(allFleetsHere.size > 1) {
        val victorId = finalGarrison.get.playerId
        val shipsRemaining = finalGarrison.get.ships
        val loserIds = allFleetsHere.map(_.playerId) - victorId
        
        Some(BattleReport(victorId, shipsRemaining, loserIds, s.id, curYear))
      } else None
      
      // 5. transfer ownership
      val s5 = finalGarrison match {
        case Some(g) => s1.copy(ownerIdOpt=Some(g.playerId), 
                                garrison=finalGarrison)
        case None => s1.copy(garrison=None)
      }
      
      val finalStar = s5
      
      (finalStar, reportOpt)
    }).unzip
    
    val newPlayers = players.map( p => {
      // collect all stars of player
      val playerStars = stars3.filter(_.ownerIdOpt==Some(p.id))
      
      // update player's stars owned count
      val newStarsOwned = playerStars.length
            
      // wealth distribution
      val totalWealthRate = 
        playerStars.map(
          _.planets.map(_.wealthYield).sum
        ).sum
      val wealthEarnedThisTick = totalWealthRate*StarGameState.tickSizeYears
      
      // Sensors include colonies AND newly arrived fleets (before battle)
      val sensors = playerStars ++ 
        newlyArrivedFleets.filter(_.playerId == p.id) 
      
      // detect stars with sensors
      val detectedStars = 
        FleetView.inRangeItems(this, p.sensorRange, sensors, stars3.toSet)
      val detectedColonizedStars = detectedStars.filter(_.ownerIdOpt.isDefined) 
      
      val detectedPlayers = detectedColonizedStars.map(_.ownerIdOpt.get).toSet
      
      // meet players
      val newMetPlayers = p.metPlayerIds union detectedPlayers
      
      // colonies of met players
      val metPlayersStarIds = stars.filter(s =>
          s.ownerIdOpt.isDefined && newMetPlayers.contains(s.ownerIdOpt.get)
        ).map(_.id).toSet
      
      val newExploredStarIds = // union of all three 
        p.exploredStarIds | detectedStars.map(_.id).toSet | metPlayersStarIds
      
      p.copy(exploredStarIds=newExploredStarIds, metPlayerIds=newMetPlayers,
             gold = p.gold+wealthEarnedThisTick, starsOwned=newStarsOwned)
    })
    
    // Check victory conditions. (Own 80% of stars)
    val gameVictor = 
      newPlayers.find(_.starsOwned.toDouble/stars.length > 0.75)
        .map(_.id)
    val gameTimeout = gameYear > 1000.0
    
    copy(gameYear=curYear, movingFleets=newMovingFleets, 
         players=newPlayers,
         stars=stars3, reports=reports ++ newReports.flatten,
         finished = gameVictor.isDefined || gameTimeout, 
         gameVictor = gameVictor.getOrElse(-1))
  }
  
  def updated() : StarGameState = {
    StarGameState.updated(this)
  }
    
  def meta = StarGameState
  
  def isOneOfThePlayers(openid: String) = 
    players.exists(_.openid == Some(openid))

}

object StarGameState extends MongoDocumentMeta[StarGameState] with Logger {
  override def collectionName = "StarGameState"
  
  override def formats = EnumSerializer.formats
  
  val starDensity = 1/36.0 // One per 36 square light years
  
  val sizesNStars   = List(8, 16, 36, 60, 108)
  val sizesAreas    = sizesNStars.map(_ / starDensity)
  val sizesSqLength = sizesAreas.map(A => math.sqrt(A))
  val sizesIndices  = List(0,1,2,3,4)  
  
  val sizesNames    = List("Tiny", "Small", "Medium", "Large", "Huge")
    .zip(sizesNStars).map(t => "%s (%d)".format(t._1, t._2))
  
  val tickSizeYears = 0.1 // how long a tick should be (in year terms) 6 minutes
  
  // size: 0=Small, 1=Medium, etc.
  // returns: newly created state
  def newState(createdBy: String, name: String = "Untitled Game", 
               yearsPerDay: Double = 1440,
               size: Int = 1,
               nPlayers: Int = 3) : StarGameState = {
    
    println("nPlayers: %d".format(nPlayers))
    println("size: %d".format(size))
    println("years per day: %f".format(yearsPerDay))
    val id = (new ObjectId).toString
    
    val numStars = sizesNStars(size)
    val mapSizeL = sizesSqLength(size)
    
    val shuffledNames = Random.shuffle(Star.names)
    
    val stars = 
      (0 until numStars).map( 
        starId => Star.getRandom(mapSizeL, starId, shuffledNames(starId)) 
      ).toList
    
    val starIdsWithTerran = 
      stars.filter( _.planets.exists(_.pType == PlanetType.Terran) ).map(_.id)
    
    if (starIdsWithTerran.length >= nPlayers) {
      // currently does no filtering to ensure players start far from each other
      StarGameState(id, createdBy, name, sizesNames(size), 
                    yearsPerDay=yearsPerDay,
                    realStartTime=new Date,
                    lastMutateTime=new Date,
                    nPlayers=nPlayers, stars=stars,
                    players=Nil, movingFleets=Set(),
                    availableStartStarIds = starIdsWithTerran)
    } else {
      info("Insufficient # of available starting planets: " + 
           "(%d/%d) found.".format(starIdsWithTerran.length, nPlayers))
      
      // try again
      newState(createdBy, name=name, yearsPerDay=yearsPerDay, size=size,
               nPlayers=nPlayers)
    }
  }
  
  @tailrec def updated(s: StarGameState) : StarGameState =
    if(!s.started || s.finished 
    || s.gameYear + tickSizeYears > s.supposedToBeYear) {
       s
    } else {
      updated(s.advancedOneTick())
    }
}




