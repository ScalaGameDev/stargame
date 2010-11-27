package impulsestorm.liftapp.model.stargame

import impulsestorm.liftapp.lib._
import net.liftweb.common.Logger

import org.bson.types.ObjectId

import scala.util.Random

import java.util.Date

import _root_.net.liftweb.mongodb._

case class StarGameState( _id: String, createdBy: String, name: String, 
                          mapSize: String, started: Boolean = false,
                          gameYear: Double = 0 , timeMultiplier: Double,
                          realStartTime: Date, nPlayers: Int,
                          stars: List[Star], players: List[Player],
                          fleets: List[Fleet], colonies: List[Colony],
                          availableStartStarIds: List[Int])
  extends State with MongoDocument[StarGameState] {
  
  def addedPlayer(pSpec: PlayerSpec) = {
    val newPlayerId = players.length // same as array index
    val homeStarId = availableStartStarIds.head
    
    val newPlayers = 
      players :+ Player.startingPlayer(newPlayerId, pSpec, homeStarId)
    
    val newColonies = 
      colonies :+ Colony.startingColony(stars(homeStarId))
    
    val newFleets =
      fleets :+ Fleet.startingFleet(newPlayerId, homeStarId)
    
    println("addedPlayer")
      
    this.copy(availableStartStarIds = availableStartStarIds.tail,
              players = newPlayers, colonies = newColonies, fleets=newFleets)
  }
  
  def updatedPlayer(p: Player) =
    this.copy(players=players.updated(p.id, p))
    
  def meta = StarGameState
  
  // what game year it should be given the current real time
  def shouldBeGameYear = {
    val secondsOld = 
      ( (new java.util.Date).getTime - realStartTime.getTime ) / 1000.0
    val daysOld = secondsOld/86400.0
      
    val newGameYear = gameYear + daysOld*timeMultiplier 
  }
  
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
  
  // size: 0=Small, 1=Medium, etc.
  // returns: newly created state
  def newState(createdBy: String, name: String = "Untitled Game", size: Int = 1,
               nPlayers: Int = 3) : StarGameState = {
    
    val id = (new ObjectId).toString
    val timeMultiplier = 24 // one year per real world hour
    
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
                    timeMultiplier=timeMultiplier,
                    realStartTime=new Date, nPlayers=nPlayers, stars=stars,
                    players=Nil, fleets=Nil, colonies=Nil,
                    availableStartStarIds = starIdsWithTerran)
    } else {
      info("Insufficient # of available starting planets: " + 
           "(%d/%d) found.".format(starIdsWithTerran.length, nPlayers))
      
      // try again
      newState(createdBy, name, size, nPlayers)
    }
  }
}




