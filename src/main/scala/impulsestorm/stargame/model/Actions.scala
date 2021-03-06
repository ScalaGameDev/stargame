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
      
      filledPlayersState
        .copy(started = true,
              realStartTime = new java.util.Date)
        .advancedOneTick() // force advance of one tick.
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
    Mutate( sender.stateId, sender, (s: StarGameState) => if(!s.finished) {
      val p = s.players(sender.player.id) 
      s.updatedPlayer(p.copy(techs=tech::p.techs, gold=p.gold-tech.cost))
    } else error(s, sender, "Game already over."))
  
  def BuildFactories(sender: StarGameComet, starId: Int, nFactories: Int) =
    Mutate( sender.stateId, sender, (s: StarGameState) => if(!s.finished) {
      val p = s.players(sender.player.id) 
      val oldStar = s.stars(starId)
      
      if(oldStar.ownerIdOpt == Some(p.id)) {
        val cost = 100
        val nCanAfford = math.min(nFactories*cost, p.gold.toInt)/cost
        val totalCost = cost*nCanAfford
        
        val newStarList = s.stars.updated(starId, 
          oldStar.copy(factories = oldStar.factories + nCanAfford))
        
        s.updatedPlayer(p.copy(gold=p.gold-totalCost)).copy(
          stars = newStarList)
      } else error(s, sender, "You do not own that star.")
    } else error(s, sender, "Game already over."))
      
  def BuildShips(sender: StarGameComet, starId: Int, nShips: Int) =
    Mutate( sender.stateId, sender, (s: StarGameState) => if(!s.finished) {
      val p = s.players(sender.player.id) 
      val oldStar = s.stars(starId)
      
      if(oldStar.ownerIdOpt == Some(p.id)) {
        val cost = 10
        val nCanAfford = math.min(nShips*cost, p.gold.toInt)/cost
        val nCanQueue = oldStar.factories*5
        val nQueued = math.min(nCanQueue, nCanAfford)
        val totalCost = cost*nQueued
        
        val newStarList = s.stars.updated(starId, 
          oldStar.copy(queuedProduction = 
            oldStar.queuedProduction + nQueued))
        
        s.updatedPlayer(p.copy(gold=p.gold-totalCost)).copy(
          stars = newStarList)
      } else error(s, sender, "You do not own that star.")
    } else error(s, sender, "Game already over."))
    
  def DispatchShips(sender: StarGameComet,
                    fromStarId: Int,
                    quantity: Int,
                    toStarId: Int) 
    = MutateHinted(sender.stateId, sender, (s: StarGameState) => 
      if(!s.finished) {
        s.stars(fromStarId).garrison match {
          case Some(oldF) => {
            val p = s.players(sender.player.id)
            
            // test whether player owns fleet
            if(oldF.playerId != p.id) 
              errorH(s, sender, "You do not own that fleet")
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
                val fromStar = s.stars(fromStarId)
                val toStar   = s.stars(toStarId)
                
                if(fromStar.distanceTo(s)(s.stars(toStarId)) > p.range)
                  errorH(s, sender, "Destination star out of range")
                else {
                
                  val departYear = s.gameYear
                  val arriveYear = 
                    s.gameYear+(fromStar.distanceTo(s)(toStar)/p.speed)
                  
                  val dispatchedFleet = 
                    oldF.copyMoving(ships=takenQs, toStarId=toStarId,
                                    departYear=departYear,                                                arriveYear=arriveYear).newUUID()
                  
                  val leftBehindFleet = 
                    if(leftBehindQs == 0) None
                    else Some(oldF.copy(ships=leftBehindQs).newUUID())
                    
                  val newMovingFleets = s.movingFleets + dispatchedFleet
                  
                  val newStars = s.stars.updated(fromStarId,
                    fromStar.copy(garrison=leftBehindFleet)) 
                  
                  val newState = 
                    s.copy(movingFleets=newMovingFleets, stars=newStars)
                  
                  (newState, Hint(true).selected(dispatchedFleet))
                }
              }
            }
          }
          case None => errorH(s, sender, "No fleet found at that star.")
        }
      } else errorH(s, sender, "Game already over."))
                    
      
  case class ActionError(msg: String)
}
