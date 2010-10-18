package impulsestorm.liftapp.model.stargame

import net.liftweb.common.SimpleActor

import impulsestorm.liftapp.lib._

object Actions {
  // Puts a new player into existence, be it AI or human
  def RegisterPlayer(stateId: String,
                     listener: SimpleActor[Any],
                     openid: Option[String],
                     alias: String,
                     traits: List[Trait]) 
    = Mutate( stateId, listener, (s : StarGameState) =>
      if(s.players.length >= s.nPlayers) {
        listener ! ActionError("Could not register. Game is full.")
        s
      } else if(s.players.exists(_.alias == alias)) {
        listener ! ActionError("Could not register. Alias taken.")
        s
      } else {
        val newPlayerId = s.players.length // same as array index
        val homeStarId = s.availableStartStarIds.head
        
        val newPlayers = 
          s.players :+ Player.startingPlayer(newPlayerId,
                                             openid, alias, 
                                             traits, homeStarId)
        
        val newColonies = 
          s.colonies :+ Colony.startingColony(s.stars(homeStarId))
        
        val newFleets =
          s.fleets :+ Fleet.startingFleet(newPlayerId, homeStarId)
        
        s.copy(s, 
               replaceAvailableStartStarIds=Some(s.availableStartStarIds.tail),
               replacePlayers = Some(newPlayers))
      })
    
  case class ActionError(msg: String)
}
