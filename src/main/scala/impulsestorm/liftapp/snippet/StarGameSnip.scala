package impulsestorm.liftapp {

package snippet {

import _root_.scala.xml.{NodeSeq, Text}
import _root_.net.liftweb.util._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._

import impulsestorm.liftapp.model.{StarGame, StarGameState}
import impulsestorm.liftapp.lib.ImOpenIDVendor

class StarGameSnip {
  def newfrm(in: NodeSeq) : NodeSeq = {
    var mapSize = 1;
    var nPlayers = 4;
    
    def handleForm() = {
      val newGameId = 
        StarGame.newState(createdBy=ImOpenIDVendor.identifier,
                          size=mapSize, nPlayers=nPlayers)
      S.redirectTo("play/" + newGameId)
    }
    
    val sizesOpts = 
      StarGame.sizesIndices zip StarGame.sizesNames
    
    // FIXME : wtf why doesn't type inferencing figure out what the type of
    // the onSubmit function argument is...
    bind("form", in,
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
          "nPlayers"->g.nPlayers,
          "status"-> (if(g.started) "In progress" else "Waiting for players"),
          "playLink"-> <a href={"play/"+g._id}>Play</a>
        )
      )
    }
    
    bind("list", in,
      "listgames" -> bindGames _)
  }
  
}

}
}
