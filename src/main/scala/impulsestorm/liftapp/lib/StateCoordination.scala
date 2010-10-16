package impulsestorm.liftapp.lib

import se.scalablesolutions.akka.actor.{Actor, ActorRef}

import net.liftweb.common.{SimpleActor, Logger}
import java.util.Date

class StateSupervisor(
  newStateMaster: (String) => Option[ActorRef], 
  SMTimeout: Int = 300) 
  extends Actor with Logger {
  
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
      case None => newStateMaster(id) match {
        case Some(sMaster) => touchAndGive(sMaster) // successful spawn
        case None => None // failure to spawn SM
      }
    }
  }
  
  def receive = {
    case msg: FwdedMsg => getStateMaster(msg.stateId) match {
      // if can get stateMaster, forward, otherwise, drop
      case Some(sMaster) => sMaster ! msg
      case None => Unit
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
      (sMaster !! PrepareShutdown) match {
        case Some(OK) => {
          sMaster.stop
        }
        case _ => throw new java.io.IOException("Failure on PrepareShutdownMsg")
      }
    })
    
    info("Cleaned old actors: " + killMap.keys.toString)
    
    killMap.keys.foreach(activeSMs.remove)
  }
  
  
}

trait FwdedMsg { val stateId: String }
case class Inquire(stateId: String, interested: SimpleActor[Any]) extends FwdedMsg
case class Subscribe(stateId: String, listener: SimpleActor[Any])
  extends FwdedMsg
case class Unsubscribe(stateId: String, listener: SimpleActor[Any])
  extends FwdedMsg
object PrepareShutdown
object OK

// Coordinates mutation, persistence, and notification of ONE state
trait StateMaster[StateType <: State] extends Actor {
  
  // Class specific mutate messages
  case class Mutate
    ( mutateF: StateType => (StateType, Any),
      stateId: String = stateId ) extends FwdedMsg
  
  val stateId: String;
      
  var state: StateType
  var listeners: List[SimpleActor[Any]] = Nil
  
  def receive = {
    case Mutate(mutateF, stateId) => { 
      val (newstate, hint) = mutateF(state)
      listeners.foreach( _ ! (newstate, hint) )
      
      state = newstate
      saveToStorage()    
    }
    case Inquire(id, interested) => {
      interested ! state
    }
    case Subscribe(id, listener) => 
      listeners = listener :: listeners // set
    case Unsubscribe(id, listener) => 
      listeners = listeners.filter(_!=listener) // set
    case PrepareShutdown =>
      saveToStorage()
      self reply OK
  }
  
  // non-atomic saving to durable storage
  def saveToStorage() 
}

trait State {
}

