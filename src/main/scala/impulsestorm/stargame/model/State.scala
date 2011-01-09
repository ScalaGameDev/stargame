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
                          realStartTime: Date,
                          
                          gameYear: Double = 0,
                          
                          yearsPerDay: Double,
                          
                          stars: List[Star], players: List[Player],
                          fleets: Set[Fleet], colonies: Set[Colony],
                          availableStartStarIds: List[Int])
  extends State[StarGameState] with MongoDocument[StarGameState] {
  
  def addedPlayer(pSpec: PlayerSpec) = {
    val newPlayerId = players.length // same as array index
    val homeStarId = availableStartStarIds.head
    
    val newPlayers = 
      players :+ Player.startingPlayer(newPlayerId, pSpec, homeStarId)
    
    val newColonies = 
      colonies + Colony.startingColony(stars(homeStarId), newPlayerId)
    
    val newFleets =
      fleets + Fleet.startingFleet(newPlayerId, stars(homeStarId))
    
    println("addedPlayer")
      
    this.copy(availableStartStarIds = availableStartStarIds.tail,
              players = newPlayers, colonies = newColonies, fleets=newFleets)
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
    
    // process fleet arrivals and merging of stationary fleets of the same team
    val fleetsArrived = fleets.map( _.updateIfArrived(curYear) )
    val fleetsArrivedAndMerged = Fleet.mergeStationaryFleets( fleetsArrived )
    
    // TODO: resolve battles and colony ownership transfers here 
    
    // exploredStarIds and income
    val newPlayers = players.map( p => {
      val stationaryOwnedFleets = 
        fleetsArrivedAndMerged.filter(f => f.playerId == p.id && !f.moving)
      
      val sensors = // our fleets and our colonies are our sensors
        stationaryOwnedFleets ++ colonies.filter(_.ownerId == p.id) 
      
      // detect stars with sensors
      val detectedStarIds = 
        FleetView.inRangeItems(this, p.sensorRange, sensors,
          stars.toSet).map(_.id)
      
      // detect colonies and subsequently, players
      val detectedColonies = colonies.filter(c => 
        detectedStarIds.contains(c.starId))
      
      val detectedPlayers = detectedColonies.map(_.ownerId).toSet
      
      // meet players
      val newMetPlayers = p.metPlayerIds union detectedPlayers
      
      // colonies of met players
      val metPlayersColonyStarIds = colonies.filter(c => 
        newMetPlayers.contains(c.ownerId)).map(_.starId)
      
      val newExploredStarIds = // union of all three 
        p.exploredStarIds | detectedStarIds.toSet | metPlayersColonyStarIds
      
      p.copy(exploredStarIds=newExploredStarIds, metPlayerIds=newMetPlayers)
    })
    
    copy(gameYear=curYear, fleets=fleetsArrivedAndMerged, players=newPlayers )
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
  
  val sizesNStars   = List(24, 48, 70, 108)
  val sizesAreas    = sizesNStars.map(_ / starDensity)
  val sizesNames    = List("Small", "Medium", "Large", "Huge")
  val sizesSqLength = sizesAreas.map(A => math.sqrt(A))
  val sizesIndices  = List(0,1,2,3)  
  
  val tickSizeYears = 0.1 // how long a tick should be (in year terms) 6 minutes
  
  // size: 0=Small, 1=Medium, etc.
  // returns: newly created state
  def newState(createdBy: String, name: String = "Untitled Game", size: Int = 1,
               nPlayers: Int = 3) : StarGameState = {
    
    val id = (new ObjectId).toString
    val yearsPerDay = 8000.0
    
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
                    realStartTime=new Date, nPlayers=nPlayers, stars=stars,
                    players=Nil, fleets=Set(), colonies=Set(),
                    availableStartStarIds = starIdsWithTerran)
    } else {
      info("Insufficient # of available starting planets: " + 
           "(%d/%d) found.".format(starIdsWithTerran.length, nPlayers))
      
      // try again
      newState(createdBy, name, size, nPlayers)
    }
  }
  
  @tailrec def updated(s: StarGameState) : StarGameState =
    if(s.gameYear + tickSizeYears > s.supposedToBeYear) s else {
      updated(s.advancedOneTick())
    }
}




