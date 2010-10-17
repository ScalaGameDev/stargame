package impulsestorm.liftapp.model.stargame
import impulsestorm.liftapp.lib._

import se.scalablesolutions.akka.actor.{Actor}

class StarGameMaster(var state: StarGameState)
  extends StateMaster[StarGameState] {
  def saveToStorage() = 
    state.save
  
  val stateId = state._id
  
  // Puts a new player into existence, be it AI or human
  def RegisterPlayer(openid: Option[String],
                     alias: String,
                     traits: List[Trait.Value]) = Mutate( s => {
    if(s.players.length < s.nPlayers) {
      val homeStarId = s.availableStartStarIds.head
      
      val newPlayers = 
        s.players :+ Player.startingPlayer(openid, alias, traits, homeStarId)
      
      val newColonies = 
        s.colonies :+ Colony.startingColony(s.stars(homeStarId))
      
      val newFleets =
        s.fleets :+ Fleet.startingFleet
      
      (s.copy(s, 
              replaceAvailableStartStarIds=Some(s.availableStartStarIds.tail),
              replacePlayers = Some(newPlayers)),
       None) 
    }
    else
      (s, None)
  })
}

object StarGameMaster {
  def spawn(id: String) = StarGameState.find(id) match {
    case Some(state) => Some(Actor.actorOf(new StarGameMaster(state)).start)
    case _ => None
  }
}
