package impulsestorm.liftapp.model.stargame

import net.liftweb.common.SimpleActor

import impulsestorm.liftapp.lib._

object Actions {
  def StartGame(stateId: String,
                sender: SimpleActor[Any])
    = Mutate( stateId, sender, (s: StarGameState) => {
      // Fill rest of slots with AI
      def fillEmptySlotsWithAI(newState: StarGameState) : StarGameState =
        if(newState.players.length < newState.nPlayers)
          fillEmptySlotsWithAI(s.addedPlayer(PlayerSpec.randomAIPlayer(s)))
        else newState
      
      val filledPlayersState = fillEmptySlotsWithAI(s)
      filledPlayersState.copy(started = true, 
                              realStartTime = new java.util.Date)
    })
                  
  
  // Puts a new player into existence, be it AI or human
  def RegisterPlayer(stateId: String,
                     sender: SimpleActor[Any],
                     pSpec: PlayerSpec) 
    = Mutate( stateId, sender, (s : StarGameState) =>
      if(s.players.length >= s.nPlayers) {
        sender ! ActionError("Could not register. Game is full.")
        s
      } else if(s.players.exists(_.alias == pSpec.alias)) {
        sender ! ActionError("Could not register. Alias taken.")
        s
      } else if(s.players.exists(_.openid == pSpec.openid)) {
        sender ! ActionError("You are already registered as a player.")
        s
      } else {
        s.addedPlayer(pSpec)
      })
  
  def ResearchAllocation(stateId: String,
                         sender: SimpleActor[Any],
                         player: Player,
                         allocation: List[Double]) 
    = Mutate( stateId, sender, (s : StarGameState) =>
        s.updatedPlayer(player.copy(researchAlloc=allocation))
      )
      
  def ResearchChoice(stateId: String,
                     sender: SimpleActor[Any],
                     player: Player,
                     newChoice: Tech) 
    = Mutate( stateId, sender, (s : StarGameState) => {
        val choices = player.researchChoices.updated(
          TechCategory.values.indexOf(newChoice.category),
          newChoice)
          
        s.updatedPlayer(player.copy(researchChoices=choices))
      })
  
  case class ActionError(msg: String)
}
