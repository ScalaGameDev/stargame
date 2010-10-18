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
  
  // copy constructor
  def copy( src: StarGameState, 
            replacePlayers: Option[List[Player]] = None,
            replaceAvailableStartStarIds: Option[List[Int]] = None,
            replaceFleets: Option[List[Fleet]] = None,
            replaceColonies: Option[List[Colony]] = None) = 
  {
    
    val players = replacePlayers getOrElse src.players
    val availableStartStarIds = 
      replaceAvailableStartStarIds getOrElse src.availableStartStarIds
    val fleets = replaceFleets getOrElse src.fleets
    val colonies = replaceColonies getOrElse src.colonies
              
    StarGameState(  _id = src._id,
                    createdBy = src.createdBy,
                    name = src.name,
                    mapSize = src.mapSize,
                    started = src.started,
                    gameYear = src.gameYear,
                    timeMultiplier = src.timeMultiplier,
                    realStartTime = src.realStartTime,
                    nPlayers = src.nPlayers,
                    stars = src.stars,
                    players = players,
                    fleets = fleets,
                    colonies = colonies,
                    availableStartStarIds = availableStartStarIds )
  }
    
  def meta = StarGameState
  
  // what game year it should be given the current real time
  def shouldBeGameYear = {
    val secondsOld = 
      ( (new java.util.Date).getTime - realStartTime.getTime ) / 1000.0
    val daysOld = secondsOld/86400.0
      
    val newGameYear = gameYear + daysOld*timeMultiplier 
  }
  
  def isOneOfThePlayers(openid: String) = 
    players.exists(_.openid.get == openid)

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
  
                     //http://www.rinkworks.com/namegen/ - Japanese
  val aliases = List("Hoshisaka", "Akisa", "Zenchi", "Uesaki", "Sawa", "Ohira",
                     "Taku", "Ruyu", "Woyu", "Tsuya", "Rumata", "Munen",
                     // Chinese (modified a bit)
                     "Zhao", "Hou", "HianLao", "Shia", "Qao", "ShouWiann", "Mao", "Xian", "Chan", "Chao", "LiBai",
                     // Greek!
                     "Athoios", "Ildeos", "Osuos", "Rilytia", "Phacios",
                     "Queyos", "Gariatia", "Atetia", "Honatia", "Nikios",
                     // Pokemon
                     "Awertle", "Kelortle", "Pinizard", "Wadochu", "Skullezard",
                     // Fantasy - vowels
                     "Iri", "Airalsti", "Yana", "Haisri", "Aedue", "Raerithp",
                     // Other fantasy
                     "Taing", "Hatmor", "Irum", "Perdis", "Roob", "Rildan",
                     "Kesale", "Droshin", "Syd", "Ispertai", "Beper", "Osack"
                     )
  
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

case class Player( id: Int, openid: Option[String], alias: String,
                   traits: List[Trait],
                   exploredStarIds: List[Int],
                   designs: List[Design],
                   gold: Double,
                   techs: List[Tech], 
                   researchAlloc: List[Double] )

object Player {
  def startingPlayer( id: Int, openid: Option[String], alias: String,
                      traits: List[Trait], startingStarId: Int) = {
    Player(id, openid, alias, traits, 
           exploredStarIds = List(startingStarId),
           gold = 0, designs = Design.startingDesigns, techs = Nil, 
           researchAlloc=TechCategory.defaultAllocation)
  }
}


