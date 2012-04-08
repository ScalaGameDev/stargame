package impulsestorm.stargame.model
import impulsestorm.stargame.lib._

import net.liftweb.common.{SimpleActor, Logger}
import akka.actor.{Actor, ActorContext, Props}
import java.util.Date

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
      state = mutateF(state.updated()).copy(lastMutateTime = new Date)
      listeners.foreach(_ ! state)
      
      saveToStorage()    
    }
    case MutateHinted(stateId, sender, mutateHintedF) => {
      val (newstate1, hint) = mutateHintedF(state.updated())
      val newstate2 = newstate1.copy(lastMutateTime = new Date)
      listeners.foreach(_ ! (newstate2, hint))
      
      state = newstate2
      saveToStorage()
    }
    case Inquire(id, sender) => {
      state = state.updated().copy(lastMutateTime = new Date)
      sender ! state
      saveToStorage()
    }
    case InquireMapUpdates(id, sender) => {
      state = state.updated().copy(lastMutateTime = new Date)
      sender ! (state, Hint(true))
    }
  }
    
  override def receive = myReceive orElse super.receive
  
  def saveToStorage() = 
    state.save
  
  val stateId = state._id
}

object StarGameMaster {
  def spawn(id: String, context: ActorContext) = {    
    if(id.length == 24) // find method should do check, but w/e
      StarGameState.find(id) match {
        case Some(state) => 
          Some(context.actorOf(Props(new StarGameMaster(state))))
        case _ => None
      }
    else
      None
  }
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
