package impulsestorm.stargame.model
import impulsestorm.stargame.lib._

import se.scalablesolutions.akka.actor.{Actor}

class StarGameMaster(var state: StarGameState)
  extends StateMaster[StarGameState] {
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
