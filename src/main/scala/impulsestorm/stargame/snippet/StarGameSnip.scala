package impulsestorm.stargame {

package snippet {

import _root_.scala.xml.{NodeSeq, Text}
import _root_.net.liftweb.util._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._

import impulsestorm.stargame.model.StarGame
import impulsestorm.stargame.model.StarGameState
import impulsestorm.stargame.lib.ImOpenIDVendor

import java.util.Date

class StarGameSnip {
  
  def newfrm(in: NodeSeq) : NodeSeq = {
    var name = "Untitled Game"
    var mapSize : Int = 1;
    var nPlayers = 3;
    var yearsPerDay : Double = 8640.0; // turbo speed for testing
    
    val speedOptions = List(
      8640.0->"Turbo: 1 yr = 10 seconds",
      1440.0->"Realtime: 1 yr = 1 min",
      144.0->"Slacking off at work: 1 yr = 10 min",
      24.0->"Casual: 1 yr = 1 hr",
      12.0->"Strategic: 1yr = 2 hr",
      6.0->"Contemplative: 1 yr = 4 hr"
    )
      
    
    def handleForm() = {
      val gameName = if(name != "") name else "Untitled Game"
      val newGameState = 
        StarGameState.newState(name=name, createdBy=ImOpenIDVendor.identifier,
                               size=mapSize, nPlayers=nPlayers,
                               yearsPerDay=yearsPerDay)
     
      newGameState.save
      
      S.redirectTo("play/" + newGameState._id)
    }
    
    val sizesOpts = 
      StarGameState.sizesIndices zip StarGameState.sizesNames
    
    // FIXME : wtf why doesn't type inferencing figure out what the type of
    // the onSubmit function argument is...
    bind("form", in,
      "name"     -> SHtml.text(name, name = _),
      "mapSize"  -> SHtml.selectObj[Int](options = sizesOpts, 
                                         default = Full(mapSize),
                                         onSubmit = mapSize = _ ),
      "nPlayers" -> SHtml.selectObj[Int](options = 
                                         (2 to 8) zip (2 to 8).map(_.toString),
                                         default = Full(nPlayers),
                                         onSubmit = nPlayers = _),
      "speed"    -> SHtml.selectObj[Double](options = speedOptions,
                                            default = Full(1440.0),
                                            onSubmit = yearsPerDay = _),
      "submit"   -> SHtml.submit("Create game", handleForm)
    )
  }
  
  def list(in: NodeSeq) : NodeSeq = {
    val allGames = StarGameState.findAll
    
    // use opportunity to cull old games
    val (toDelete, goodGames) = allGames.partition({ state =>
      val msSinceLastMutate = (new Date).getTime - state.lastMutateTime.getTime
      val daysSinceLastMutate = msSinceLastMutate/(86400*1000)
      
      daysSinceLastMutate > 4
    })
    toDelete.foreach(_.delete)
    
    val (myGames, otherGames) =   
      goodGames.partition(g =>
        g.createdBy == ImOpenIDVendor.identifier ||
        g.isOneOfThePlayers(ImOpenIDVendor.identifier))
    
    val awaitingGames = otherGames.filter(!_.started)
    
    def bindGames(games: List[StarGameState])(template: NodeSeq) : NodeSeq = {
      games.flatMap( g => {
        val status = if(g.finished) {
          if(g.gameVictor != -1)
            "Game over: Victory: %s".format(g.players(g.gameVictor).alias)
          else
            "Game over: Draw"
        }
        else if(g.started) 
          "In progress" 
        else 
          "Waiting for players"
        
        bind("game", template,
          "name"->g.name,
          "mapSize"->g.mapSize,
          "nPlayers"->"%d/%d".format(g.players.length, g.nPlayers),
          "status"-> status,
          "playLink"-> <a href={"/stargame/play/"+g._id}>Play</a>
        )
      })
    }
    
    {
      if(!myGames.isEmpty)
        bind("list", in,
          "tableTitle" -> "My games",
          "listgames" -> bindGames(myGames) _)
      else
        <div></div>
    } ++ {
      if(!awaitingGames.isEmpty)      
        bind("list", in,
          "tableTitle" -> "Games awaiting players",
          "listgames" -> bindGames(awaitingGames) _)
      else
        <div></div>
    }
    
  }
  
  def play(in: NodeSeq) : NodeSeq = S.param("gameId") match {
    case Full(gameId) => {
      val cometName = 
        ImOpenIDVendor.identifier + gameId + java.util.UUID.randomUUID.toString; 
      <lift:comet type="StarGameComet" name={ cometName }>
        <comet:message />
      </lift:comet>
    }
    case _ => S.redirectTo("/stargame/")
  }
  
  def stargameRedirect(in: NodeSeq) = {
    if(ImOpenIDVendor.loggedIn)
      S.redirectTo("/stargame/")
    <div/>
  }
}

}
}
