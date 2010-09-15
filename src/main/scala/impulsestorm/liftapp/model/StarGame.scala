package impulsestorm.liftapp.model

import _root_.net.liftweb.mongodb._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import impulsestorm.liftapp.lib._

import java.util.Date
import se.scalablesolutions.akka.actor.{Actor}
import org.bson.types.ObjectId
import scala.util.Random

case class Star( id: Long, name: String, sClass: String, x: Double, y: Double,
                 planets: List[Planet])

case class Planet( id: Long, name: Option[String], pType: String, 
                   distance: Double, baseEnvironment: Double )
                 
case class Player( openid: String, alias: Option[String],
                   exploredStarIds: List[Long],
                   gold: Double,
                   colonies: List[Colony],
                   fleetsStationary: List[FleetStationary],
                   fleetsMoving: List[FleetMoving],
                   techs: List[TechKey], 
                   researchAlloc: Map[TechCategory.Value, Double]) 

case class TechKey(key: String)
object TechCategory extends Enumeration {
  
  val Ecology = Value("Ecology")
  val Industry = Value("Industry")
  val Society = Value("Society")
  val Weaponry = Value("Weaponry")
  val Shielding = Value("Shielding")
  val Propulsion = Value("Propulsion")
}

case class Colony( starId: Long, settlements: List[Settlement] )

case class Settlement( planetId: Long, population: Double, capital: Double )

// no location: in transit
case class FleetStationary( ships: Map[ShipDesign, Long], locationStarId: Long )

case class FleetMoving( ships: Map[ShipDesign, Long], fromStarId: Long, toStarId: Long,
                        departClock: Double, arriveClock: Double )

case class ShipDesign( warpSpeed: Double )                        

case class StarGameState( _id: String, 
                          name: String, 
                          gameYear: Double, timeMultiplier: Double,
                          realWorldTime: Date,
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
  def mutate(mutationData: Any) = (state, None)
}

object StarGame {
  
  val starClasses : List[(Double, String)] =
    List(0.13->"B", 0.6->"A", 3.0->"F", 7.6->"G", 12.1->"K", 76.45->"M")
  
  def newState(name: String = "Untitled Game") = {
    val id = new ObjectId
    val timeMultiplier = 24 // one year per real world hour
    
    val starDensity = 1/36.0 // One per 36 square light years
    
    val mapSize = (600.0, 600.0)
    
    val numStars = (mapSize._1*mapSize._2*starDensity).toInt
    
    val stars = (1 to numStars).map( id => {
      Star(id, name="Star"+id.toString, 
           sClass=SimRandom.weightedRandom(starClasses),
           x=Random.nextDouble*mapSize._1, y=Random.nextDouble*mapSize._2,
           planets = Nil)
           
    })
    
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
  val join = Menu(Loc("stargame-join", 
                      List("stargame", "join")->true,
                      "JoinGame", loginFirst, Hidden))
  val play = Menu(Loc("stargame-play", 
                      List("stargame", "play")->true,
                      "PlayGame", loginFirst, Hidden))
  
  val root = Menu(Loc("stargame", List("stargame", "index"), "StarGame"), 
                  newg)
                      
  val menus = List(root)
  
}

