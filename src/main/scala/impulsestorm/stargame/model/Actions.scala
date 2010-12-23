package impulsestorm.stargame.model.stargame

import impulsestorm.stargame.comet.StarGameComet

import net.liftweb.common.SimpleActor

import impulsestorm.stargame.lib._

object ActionUtils {
  def error(s: StarGameState, sender: StarGameComet, msg: String) = {
    sender ! Actions.ActionError(msg)
    s
  }
}

object Actions {
  import ActionUtils.error
  
  def StartGame(sender: StarGameComet)
    = Mutate( sender.stateId, sender, (s: StarGameState) => if(!s.started) {
      // Fill rest of slots with AI
      def fillEmptySlotsWithAI(newState: StarGameState) : StarGameState =
        if(newState.players.length < newState.nPlayers)
          fillEmptySlotsWithAI(
            newState.addedPlayer(PlayerSpec.randomAIPlayer(newState)))
        else newState
      
      val filledPlayersState = fillEmptySlotsWithAI(s)
      filledPlayersState.copy(started = true, 
                              realStartTime = new java.util.Date)
    } else s)
                  
  
  // Puts a new player into existence, be it AI or human
  def RegisterPlayer(sender: StarGameComet,
                     pSpec: PlayerSpec) 
    = Mutate( sender.stateId, sender, (s : StarGameState) =>
      if(s.players.length >= s.nPlayers)
        error(s, sender, "Could not register. Game is full.")
      else if(s.players.exists(_.alias == pSpec.alias))
        error(s, sender, "Could not register. Alias taken.")
      else if(s.players.exists(_.openid == pSpec.openid))
        error(s, sender, "You are already registered as a player.")
      else 
        s.addedPlayer(pSpec)
      )
  
  def ResearchAllocation(sender: StarGameComet,
                         allocation: List[Double]) 
    = Mutate( sender.stateId, sender, (s : StarGameState) => {
        val positiveAlloc = allocation.map(a => math.max(0, a))
        val posSum = positiveAlloc.sum
        val normalizedAlloc = positiveAlloc.map(_/posSum)
        s.updatedPlayer(sender.player.copy(researchAlloc=normalizedAlloc))  
      })
      
  def ResearchChoice(sender: StarGameComet,
                     newChoice: Tech) 
    = Mutate( sender.stateId, sender, (s : StarGameState) => {
        val choices = sender.player.researchChoices.updated(
          TechCategory.values.indexOf(newChoice.category),
          newChoice)
        s.updatedPlayer(sender.player.copy(researchChoices=choices))
      })
  
  def DispatchFleet(sender: StarGameComet,
                    fleetUuid: String,
                    quantities: List[Int],
                    toStarId: Int) 
    = Mutate(sender.stateId, sender, (s: StarGameState) => {
      s.fleets.find(_.uuid == fleetUuid) match {
        case Some(oldF) => {
          // test whether player owns fleet
          if(oldF.playerId != sender.player.id) 
            error(s, sender, "You do not own that fleet")
          else if(oldF.moving)
            error(s, sender, "Fleet already moving")
          else if(oldF.fromStarId == toStarId)
            error(s, sender, "Already stationed at star")
          else if(oldF.distanceTo(s)(s.stars(toStarId)) >      
                  sender.player.shipRange)
            error(s, sender, "Destination star out of range")
          else {
            // take the minimum between requested and actually existing
            val (takenQs, leftBehindQs) = (oldF.ships zip quantities).map {
              case (available, requested) => { 
                val taken = math.max(0, math.min(available, requested))
                (taken, available-taken)
              }
            }.unzip
            
            // must take finite amount
            if(takenQs == Fleet.emptyQuantity)
              error(s, sender, "Cannot dispatch a fleet with 0 ships")
            else {
            
              val fleetSpeed : Double = 
                oldF.copy(ships=takenQs).speed(s)
              
              val departYear = Some(s.gameYear)
              val arriveYear = 
                Some(s.gameYear+oldF.distanceTo(s)
                  (s.stars(toStarId))/fleetSpeed)
              
              val dispatchedFleet = oldF.copy(ships=takenQs, moving=true,
                                              toStarId=Some(toStarId),
                                              departYear=departYear, 
                                              arriveYear=arriveYear).newUUID
              
              val leftBehindFleet = if(leftBehindQs == Fleet.emptyQuantity) None
                else Some(oldF.copy(ships=leftBehindQs).newUUID)
                
              val newFleets = dispatchedFleet :: leftBehindFleet.toList
              
              s.copy(fleets=s.fleets.filter(_.uuid != oldF.uuid) ::: newFleets)
            }
          }
        }
        case None => error(s, sender, "No fleet found with that UUID")
      }
        
      })
                    
      
  case class ActionError(msg: String)
}
