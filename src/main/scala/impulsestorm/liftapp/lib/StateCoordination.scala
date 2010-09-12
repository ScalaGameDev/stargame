package impulsestorm.liftapp.lib

import se.scalablesolutions.akka.actor.{Actor, ActorRef}

import net.liftweb.common.{SimpleActor, Logger}
import java.util.Date

class StateSupervisor(
  newSMActor: (String) => ActorRef, SMTimeout: Int = 300) extends Actor
  with Logger {
  
  object CleanOldActorsMsg
    
  private var activeSMs = 
    scala.collection.mutable.HashMap[String, (Long, ActorRef)]()
  
  def spawnStateMaster(id: String) = {
    info("Spawned new StateMaster with ID: " + id) 
    ((new Date).getTime, newSMActor(id))
  }
  
  def receive = {
    case msg: FwdedMsg => {
      val id = msg.id
      val sMaster = 
        activeSMs.getOrElseUpdate(id, spawnStateMaster(id))._2
      
      sMaster forward msg
      
      activeSMs.update(id, ((new Date).getTime, sMaster))
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
      (sMaster !! PrepareShutdownMsg) match {
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

trait FwdedMsg { val id: String }
case class MutateMsg(id: String, mutationData: Any) extends FwdedMsg
case class SubscribeMsg(id: String, listener: SimpleActor[Any])
  extends FwdedMsg
case class UnsubscribeMsg(id: String, listener: SimpleActor[Any])
  extends FwdedMsg
object PrepareShutdownMsg
object OK

// Coordinates mutation, persistence, and notification of ONE state
trait StateMaster[StateType <: State] extends Actor {
  
  var state: StateType
  var listeners: List[SimpleActor[Any]] = Nil
  
  def receive = {
    case MutateMsg(id, mutationData) => { 
      val (newstate, hint) = mutate(mutationData)
      state = newstate
      listeners.foreach( _ ! (newstate, hint) )
    }
    case SubscribeMsg(id, listener) => 
      listeners = listener :: listeners // set
    case UnsubscribeMsg(id, listener) => 
      listeners = listeners.filter(_!=listener) // set
    case PrepareShutdownMsg =>
      saveToStorage()
      self reply OK
  }
  
  def mutate(mutationData: Any) : (StateType, Any)
  
  // non-atomic saving to durable storage
  def saveToStorage() 
}

trait State {
}

