package impulsestorm.stargame.model

import impulsestorm.stargame.comet.StarGameComet

import net.liftweb.common.SimpleActor

import impulsestorm.stargame.lib._

object ActionUtils {
  def error(s: StarGameState, sender: StarGameComet, msg: String) = {
    sender ! Actions.ActionError(msg)
    s
  }
  
  def errorH(s: StarGameState, sender: StarGameComet, msg: String) = {
    sender ! Actions.ActionError(msg)
    (s, Hint(false))
  }
}

object Actions {
  import ActionUtils._
  
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
  
  def BuyTech(sender: StarGameComet, tech: Tech) =
    Mutate( sender.stateId, sender, (s: StarGameState) => {
      val p = s.players(sender.player.id) 
      s.updatedPlayer(p.copy(techs=tech::p.techs, gold=p.gold-tech.cost,
        canResearchTechs=p.canResearchTechs.map(_.filter(_!=tech))))
    })
      
  def DispatchShips(sender: StarGameComet,
                    fleetUuid: String,
                    quantity: Int,
                    toStarId: Int) 
    = MutateHinted(sender.stateId, sender, (s: StarGameState) => {
      s.fleets.find(_.uuid == fleetUuid) match {
        case Some(oldF) => {
          val p = s.players(sender.player.id)
          
          // test whether player owns fleet
          if(oldF.playerId != p.id) 
            errorH(s, sender, "You do not own that fleet")
          else if(oldF.moving)
            errorH(s, sender, "Fleet already moving")
          else if(oldF.fromStarId == toStarId)
            errorH(s, sender, "Already stationed at star")
          else {
            // take the minimum between requested and actually existing
            val (takenQs, leftBehindQs) = {
              val taken = math.max(0, math.min(oldF.ships, quantity))
              (taken, oldF.ships-taken)
            }
            
            // must take finite amount
            if(takenQs == 0)
              errorH(s, sender, "Cannot dispatch a fleet with 0 ships")
            else {
              val takenFleetStationary = oldF.copy(ships=takenQs)
              
              if(oldF.distanceTo(s)(s.stars(toStarId)) > p.range)
                errorH(s, sender, "Destination star out of range")
              else {
              
                val departYear = Some(s.gameYear)
                val arriveYear = 
                  Some(s.gameYear+oldF.distanceTo(s)
                    (s.stars(toStarId))/p.speed)
                
                val dispatchedFleet = oldF.copy(ships=takenQs, moving=true,
                                                toStarId=Some(toStarId),
                                                departYear=departYear, 
                                                arriveYear=arriveYear).newUUID
                
                val leftBehindFleet = 
                  if(leftBehindQs == 0) None
                  else Some(oldF.copy(ships=leftBehindQs).newUUID)
                  
                val newFleets = 
                  (dispatchedFleet :: leftBehindFleet.toList).toSet
                
                val newState = 
                  s.copy(fleets=s.fleets.filter(_.uuid != oldF.uuid) union
                    newFleets)
                
                (newState, Hint(true).selected(dispatchedFleet))
              }
            }
          }
        }
        case None => errorH(s, sender, "No fleet found with that UUID")
      }
        
      })
                    
      
  case class ActionError(msg: String)
}
