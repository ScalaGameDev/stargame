package impulsestorm.stargame.model
import impulsestorm.stargame.lib._

import net.liftweb.common.{SimpleActor, Logger}
import se.scalablesolutions.akka.actor.{Actor}

case class Inquire(stateId: String, sender: SimpleActor[Any]) 
  extends FwdedMsg
case class InquireMapUpdates(stateId: String, sender: SimpleActor[Any]) 
  extends FwdedMsg

case class Mutate(stateId: String, sender: SimpleActor[Any],
                  mutateF: StarGameState => StarGameState)
  extends FwdedMsg

case class MutateHinted[StateType](stateId: String, sender: SimpleActor[Any],
                  mutateHintedF: StarGameState => (StarGameState, Hint))
  extends FwdedMsg

class StarGameMaster(var state: StarGameState)
  extends StateMaster[StarGameState] {
  
  def myReceive: PartialFunction[Any, Unit] = {
    case Mutate(stateId, sender, mutateF) => { 
      state = mutateF(state.updated())
      listeners.foreach(_ ! state)
      
      saveToStorage()    
    }
    case MutateHinted(stateId, sender, mutateHintedF) => {
      val (newstate, hint) = mutateHintedF(state.updated())
      listeners.foreach(_ ! (newstate, hint))
      
      state = newstate
      saveToStorage()
    }
    case Inquire(id, sender) => {
      state = state.updated()
      sender ! state
      saveToStorage()
    }
    case InquireMapUpdates(id, sender) => {
      state = state.updated()
      sender ! (state, Hint(true))
    }
  }
    
  override def receive = myReceive orElse super.receive
  
  def saveToStorage() = 
    state.save
  
  val stateId = state._id
}

object StarGameMaster {
  def spawn(id: String) = 
    if(id.length == 24) // find method should do check, but w/e
      StarGameState.find(id) match {
        case Some(state) => 
          Some(Actor.actorOf(new StarGameMaster(state)).start)
        case _ => None
      }
    else
      None
}

trait hasPosition {
  def position(s:StarGameState) : (Double, Double)
  
  import math._
  
  def distanceTo(s:StarGameState)(other: hasPosition) = {
    val (x1, y1) = this.position(s)
    val (x2, y2) = other.position(s)
    sqrt(pow(x2-x1,2)+pow(y2-y1,2))
  }
}

trait hasPositionSimple extends hasPosition {
  val x: Double
  val y: Double
  def position(s:StarGameState) = (x,y)
}
