package impulsestorm.liftapp.model

import _root_.net.liftweb.mongodb._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import impulsestorm.liftapp.lib._

import java.util.Date
import se.scalablesolutions.akka.actor.{Actor}
import org.bson.types.ObjectId
import scala.util.Random

case class Star( id: Long, name: String, sClass: StarClass.Value, 
                 x: Double, y: Double, planets: List[Planet])
                 
object Star {
  
  import StarClass._
  
  def getRandom( mapSizeL: Double, id: Long ) = {
    val sClass = SimRandom.weightedRandom(StarClass.abundances)
    val sName = "S"+id.toString
    
    
    Star(id, name=sName, sClass=sClass,
         x=Random.nextDouble*mapSizeL, y=Random.nextDouble*mapSizeL,
         planets = randomPlanets(sClass))
  }
  
  def randomPlanets( sClass: StarClass.Value ) : List[Planet] = {
    val nPlanets = StarClass.randomNPlanets(sClass)
    
    val zones = 
      List.fill(nPlanets)(StarClass.randomZone(sClass, nPlanets)).sorted
    
    val idZonePairs = (1 to nPlanets).toList zip zones 
    
    idZonePairs.map(p => Planet.genRandom(p._1, sClass, p._2))
  }
  
}

case class Planet( id: Long, pType: PlanetType.Value, 
                   zone: PlanetZone.Value )

object Planet {
  def genRandom( id: Long, sClass: StarClass.Value, zone: PlanetZone.Value ) = {
    val pType = PlanetZone.randomPType(sClass, zone)
    Planet(id, pType, zone)
  }
}
                   
case class Player( openid: String, alias: Option[String],
                   traits: List[Trait.Value],
                   exploredStarIds: List[Long],
                   gold: Double,
                   colonies: List[Colony],
                   fleetsStationary: List[FleetStationary],
                   fleetsMoving: List[FleetMoving],
                   techs: List[Tech.Value], 
                   researchAlloc: Map[TechCategory.Value, Double] )

case class Colony( starId: Long, settlements: List[Settlement] )

case class Settlement( planetId: Long, population: Double, capital: Double )

// no location: in transit
case class FleetStationary( ships: Map[ShipDesign, Long], locationStarId: Long )

case class FleetMoving( ships: Map[ShipDesign, Long], 
                        fromStarId: Long, toStarId: Long,
                        departClock: Double, arriveClock: Double )

case class ShipDesign( warpSpeed: Double )                        

case class StarGameState( _id: String, createdBy: String, name: String, 
                          mapSize: String, started: Boolean = false,
                          gameYear: Double = 0 , timeMultiplier: Double,
                          realWorldTime: Date, nPlayers: Int,
                          stars: List[Star], players: List[Player] )
  extends State with MongoDocument[StarGameState] {
  
  def meta = StarGameState
  
  def newGameYear = {
    val secondsOld = 
      ( (new java.util.Date).getTime - realWorldTime.getTime ) / 1000.0
    val daysOld = secondsOld/86400.0
      
    val newGameYear = gameYear + daysOld*timeMultiplier 
  }
  
}

object StarGameState extends MongoDocumentMeta[StarGameState] {
  override def collectionName = "StarGameState"
}

class StarGameMaster(var state: StarGameState)
  extends StateMaster[StarGameState] {
  def saveToStorage() = 
    state.save
  
  def readFromStorage(id: String) = 
    state = StarGameState.find(id).get
    
  // FIXME stub
  def mutate(mutationData: Any) = {
    val newState = state
    saveToStorage()
    (newState, None)
  }
}

object StarGame {
  
  val starDensity = 1/36.0 // One per 36 square light years
  
  val sizesNStars   = List(24, 48, 70, 108)
  val sizesAreas    = sizesNStars.map(_ / starDensity)
  val sizesNames    = List("Small", "Medium", "Large", "Huge")
  val sizesSqLength = sizesAreas.map(A => math.sqrt(A))
  val sizesIndices  = List(0,1,2,3)  
  
  // size: 0=Small, 1=Medium, etc.
  def newState(createdBy: String, name: String = "Untitled Game", size: Int = 1,
               nPlayers: Int = 3) = {
    
    val id = (new ObjectId).toString
    val timeMultiplier = 24 // one year per real world hour
    
    val numStars = sizesNStars(size)
    val mapSizeL = sizesSqLength(size)
    
    val stars = (1 to numStars).map( Star.getRandom(mapSizeL, _) ).toList
    
    val state =
      StarGameState(id, createdBy, name, sizesNames(size), 
                    timeMultiplier=timeMultiplier,
                    realWorldTime=new Date, nPlayers=nPlayers, stars=stars,
                    players=Nil)
    
    state.save    
    
    id
  }
  
  val newSMActor = (id: String) => 
    Actor.actorOf(new StarGameMaster(StarGameState.find(id).get))
  
  val supervisor = Actor.actorOf(new StateSupervisor(newSMActor))
  
  
}

object StarView {
  import impulsestorm.liftapp.lib.ImOpenIDVendor.loginFirst
  
  val newg = Menu(Loc("stargame-new", 
                      List("stargame", "new"),
                      "NewGame", loginFirst, Hidden))
  val play = Menu(Loc("stargame-play", 
                      List("stargame", "play")->true,
                      "PlayGame", loginFirst, Hidden))
  
  val root = Menu(Loc("stargame", List("stargame", "index"), "StarGame"), 
                  newg)
                      
  val menus = List(root)
  
}

