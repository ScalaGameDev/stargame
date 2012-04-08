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

class StarGameSnip {
  
  def newfrm(in: NodeSeq) : NodeSeq = {
    var name = "Untitled Game"
    var mapSize = 1;
    var nPlayers = 3;
    var yearsPerDay : Double = 8640.0; // turbo speed for testing
    
    val speedOptions = List(
      8640.0->"Turbo: 1 yr = 10 seconds",
      1440.0->"Realtime: 1 yr = 1 min",
      144.0->"Slacking off at work: 1 yr = 10 min",
      24.0->"Casual: 1 yr = 1 hr",
      12.0->"Strategic: 1yr = 2 hr",
      6.0->"Contemplative: 1 yr = 4 hr",
      2.0->"Slow: 1 yr = 12 hr"
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
    
    def bindGames(template: NodeSeq) : NodeSeq = {
      allGames.flatMap( g => {
        val status = if(g.finished)
          "Victory: %s".format(g.players(g.gameVictor).openid.getOrElse("AI"))
        else if(g.started) 
          "In progress" 
        else 
          "Waiting for players"
        
        bind("game", template,
          "name"->g.name,
          "mapSize"->g.mapSize,
          "nPlayers"->"%d/%d".format(g.players.length, g.nPlayers),
          "status"-> status,
          "playLink"-> <a href={"play/"+g._id}>Play</a>
        )
      })
    }
    
    bind("list", in,
      "listgames" -> bindGames _)
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
}

}
}
