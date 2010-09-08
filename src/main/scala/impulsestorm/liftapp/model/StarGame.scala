package impulsestorm.liftapp.model

import _root_.net.liftweb.mongodb._ 
import impulsestorm.liftapp.lib._

import java.util.Date
import se.scalablesolutions.akka.actor.Actor

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
                   researchAlloc: Map[TechCategoryKey, Double]) 

case class TechKey(key: String)
case class TechCategoryKey(key: String)

case class Colony( starId: Long, settlements: List[Settlement] )

case class Settlement( planetId: Long, population: Double, capital: Double )

// no location: in transit
case class FleetStationary( ships: Map[ShipDesign, Long], locationStarId: Long )

case class FleetMoving( ships: Map[ShipDesign, Long], fromStarId: Long, toStarId: Long,
                        departClock: Double, arriveClock: Double )

case class ShipDesign( warpSpeed: Double )                        

case class StarGameState( name: String, clock: Double, clockRate: Double,
                          associatedTime: Date,
                          stars: List[Star], players: List[Player] )
  extends State with MongoDocument[StarGameState] {
  
  def meta = StarGameState
  
  def newClock = {
    val secondsOld = 
      ( (new java.util.Date).getTime - associatedTime.getTime ) / 1000
    secondsOld * clockRate
  }
}

object StarGameState extends MongoDocumentMeta[StarGameState] {
  override def collectionName = "StarGameState"
}

class StarGameMaster extends StateMaster[StarGameState] {
}

object StarGame {
  
  //FIXME - temp hack to get it to compile
  def newSMActor = Actor.actorOf(new StarGameMaster)
  
  val supervisor = Actor.actorOf(new StateSupervisor(s => newSMActor))
  
  
}

