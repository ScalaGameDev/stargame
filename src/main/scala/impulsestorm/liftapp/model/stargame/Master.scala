package impulsestorm.liftapp.model.stargame
import impulsestorm.liftapp.lib._

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
