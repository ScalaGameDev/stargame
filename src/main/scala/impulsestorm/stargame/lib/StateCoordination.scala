package impulsestorm.stargame.lib

import akka.actor._
import akka.util.duration._
import akka.util.Timeout

import net.liftweb.common.{SimpleActor, Logger}
import java.util.Date
import akka.pattern.ask

class StateSupervisor(
  newStateMaster: (String, ActorContext) => Option[ActorRef], 
  SMTimeout: Int = 300) 
  extends Actor with Logger 
{
    
  implicit val timeout = Timeout(5 seconds)
  
  object CleanOldActorsMsg
    
  private var activeSMs = 
    scala.collection.mutable.HashMap[String, (Long, ActorRef)]()
  
  // gets the state master if can, spawning if needed. On failure, fail.
  // on a successful get, will touch the timestamp
  def getStateMaster(id: String) = {
    
    def touchAndGive(sMaster: ActorRef) = {
      activeSMs.update(id, ((new Date).getTime, sMaster))
      Some(sMaster)
    }
    
    activeSMs.get(id) match {
      case Some((_, sMaster)) => touchAndGive(sMaster) // already in map
      case None => newStateMaster(id, context) match {
        case Some(sMaster) => touchAndGive(sMaster) // successful spawn
        case None => None // failure to spawn SM
      }
    }
  }
  
  def receive = {
    case msg: FwdedMsg => getStateMaster(msg.stateId) match {
      // if can get stateMaster, forward, otherwise, drop
      case Some(sMaster) => sMaster ! msg
      case None => {
        error("Requested: %s, but no such game".format(msg.stateId))
        msg.sender ! NoSuchGame
      }
    }
    case CleanOldActorsMsg => cleanOldActors() 
    case _ => error("Unknown message received by StateSupervisor")
  }
  
  def cleanOldActors() = {
    val now = (new Date).getTime
    
    // filter out all the ones which are more stale than SMTimeout
    val killMap = activeSMs.filter( kvtup => now - kvtup._2._1 > SMTimeout)
    
    killMap.values.foreach(dateSMasterTuple => {
      val sMaster = dateSMasterTuple._2
      // send the PrepareShutdownMsg and stop the actors.
      (sMaster ? PrepareShutdown) onComplete {
        case Left(throwable) => throw throwable
        case Right(OK) => {
          sMaster ! PoisonPill
        }
      }
    })
    
    info("Cleaned old actors: " + killMap.keys.toString)
    
    killMap.keys.foreach(activeSMs.remove)
  }
  
  
}

trait FwdedMsg { 
  val stateId: String
  val sender: SimpleActor[Any]
}

case class Subscribe(stateId: String, sender: SimpleActor[Any])
  extends FwdedMsg
case class Unsubscribe(stateId: String, sender: SimpleActor[Any])
  extends FwdedMsg

object NoSuchGame
  
object PrepareShutdown
object OK

// Coordinates mutation, persistence, and notification of ONE state
trait StateMaster[StateType <: State[StateType]] extends Actor {
      
  var state: StateType
  var listeners: List[SimpleActor[Any]] = Nil
  
  def receive = {
    case Subscribe(id, sender) => 
      listeners = sender :: listeners // set
    case Unsubscribe(id, sender) => 
      listeners = listeners.filter(_!=sender) // set
    case PrepareShutdown =>
      saveToStorage()
      sender ! OK
  }
  
  // non-atomic saving to durable storage
  def saveToStorage() 
}

trait State[T] {
  def updated() : T
}

