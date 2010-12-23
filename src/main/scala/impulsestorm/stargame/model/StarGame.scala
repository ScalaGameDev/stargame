package impulsestorm.stargame.model

import impulsestorm.stargame.model.stargame._

import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import net.liftweb.common.Logger

import _root_.impulsestorm.stargame.lib._

import scala.util.Random

import se.scalablesolutions.akka.actor.{Actor}

object StarGame {  
  val supervisor = 
    Actor.actorOf(new StateSupervisor(StarGameMaster.spawn _)).start
}

object StarView {
  import impulsestorm.stargame.lib.ImOpenIDVendor.loginFirst
                      
  val root = Menu("Stargame") / "stargame" / "index" submenus (
    Menu("New Stargame") / "stargame" / "new" >> loginFirst >> Hidden,
    Menu("Play Stargame") / "stargame" / "play" / ** >> loginFirst >> Hidden
  )

  val menus = List(root)  
    
  val rewrites = NamedPF[RewriteRequest, RewriteResponse]("StarGame") {
    case RewriteRequest(
          ParsePath("stargame" :: "play" :: gameId :: Nil, _, _, _), _, _)
          if gameId != "index" =>
      RewriteResponse(
        "stargame" :: "play" :: Nil, Map("gameId"->gameId))
  }
  
}

