package impulsestorm.stargame {

package snippet {

import _root_.scala.xml.{NodeSeq, Text}
import _root_.net.liftweb.util._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._

import impulsestorm.stargame.model.StarGame
import impulsestorm.stargame.model.stargame.StarGameState
import impulsestorm.stargame.lib.ImOpenIDVendor

class StarGameSnip {
  
  def newfrm(in: NodeSeq) : NodeSeq = {
    var name = "Untitled Game"
    var mapSize = 1;
    var nPlayers = 4;
    
    def handleForm() = {
      val gameName = if(name != "") name else "Untitled Game"
      val newGameState = 
        StarGameState.newState(name=name, createdBy=ImOpenIDVendor.identifier,
                               size=mapSize, nPlayers=nPlayers)
     
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
                                         (1 to 8) zip (1 to 8).map(_.toString),
                                         default = Full(nPlayers),
                                         onSubmit = nPlayers = _),
      "submit"   -> SHtml.submit("Create game", handleForm)
    )
  }
  
  def list(in: NodeSeq) : NodeSeq = {
    
    val allGames = StarGameState.findAll
    
    def bindGames(template: NodeSeq) : NodeSeq = {
      allGames.flatMap( g => 
        bind("game", template,
          "name"->g.name,
          "mapSize"->g.mapSize,
          "nPlayers"->"%d/%d".format(g.players.length, g.nPlayers),
          "status"-> (if(g.started) "In progress" else "Waiting for players"),
          "playLink"-> <a href={"play/"+g._id}>Play</a>
        )
      )
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
