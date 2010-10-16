package impulsestorm.liftapp.model
import impulsestorm.liftapp.lib._

import se.scalablesolutions.akka.actor.{Actor}

class StarGameMaster(var state: StarGameState)
  extends StateMaster[StarGameState] {
  def saveToStorage() = 
    state.save
  
  def readFromStorage(id: String) = 
    state = StarGameState.find(id).get
  
  def RegisterPlayer(openid: String) = Mutate( s => {
    (s, None)
  })
}

object StarGameMaster {
  def spawn(id: String) = StarGameState.find(id) match {
    case Some(state) => Some(Actor.actorOf(new StarGameMaster(state)).start)
    case _ => None
  }
}
