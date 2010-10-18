package impulsestorm.liftapp.model.stargame

import impulsestorm.liftapp.lib._

object Actions {
  // Puts a new player into existence, be it AI or human
  def RegisterPlayer(stateId: String, openid: Option[String],
                     alias: String,
                     traits: List[Trait]) 
    = Mutate( stateId, (s : StarGameState) => {
      if(s.players.length < s.nPlayers) {
        if(s.players.contains(_.alias == alias))
          (s, None)
        
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
        
        (s.copy(s, 
                replaceAvailableStartStarIds=Some(s.availableStartStarIds.tail),
                replacePlayers = Some(newPlayers)),
         None) 
      }
      else
        (s, None)
    })
    
  object RegisterPlayerAliasTaken
}
