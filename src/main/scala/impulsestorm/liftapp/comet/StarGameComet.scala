package impulsestorm.liftapp.comet

import net.liftweb._
import http._
import http.js._
import http.SHtml._

import net.liftweb.actor._
import net.liftweb.common.{Box, Full, Logger}

import scala.xml._
import scala.util.Random

import impulsestorm.liftapp.model._
import impulsestorm.liftapp.lib._

class StarGameComet extends CometActor with Logger {
  val user = ImOpenIDVendor.identifier
  val stateId = S.param("gameId").get 
  
  var lastState: Option[StarGameState] = None
  
  override def defaultPrefix = Full("comet")
  
  override def localSetup() = {
    StarGame.supervisor ! Subscribe(stateId, this)
    StarGame.supervisor ! Inquire(stateId, this)
  }
  
  override def localShutdown() = {
    StarGame.supervisor ! Unsubscribe(stateId, this)
  }
  
  override def lowPriority = {
    case state: StarGameState => {
      lastState = Some(state)
      reRender
    }
  }
  
  def listExistingPlayers : NodeSeq = {
    val players = lastState.get.players
    <div id="player-list">
      <h3>Existing players</h3>
      <table>
        {
          if(players.isEmpty)
            <tr><td /><td>None</td></tr>
          else
            players.map( p => <tr><td></td><td>{p.alias}</td></tr> )
        }
      </table>
    </div>
  }
  
  def renderPlayerView = <p>PlayerView</p>
  
  def renderObserverView = {
    (<h2>{"Game '%s' in progress".format(lastState.get.name)}</h2> ++
     listExistingPlayers )
  }
  
  def renderJoinView = {
    val state = lastState.get
    var alias : String = 
      SimRandom.randomNoCollisions(StarGameState.aliases, 
                                   state.players.map(_.alias))
    
    var traits = Random.shuffle(Trait.values) take 2
                                   
    def processJoinForm(alias: String) = {
      info("Registered : " + alias)
    }
    
    (
      <h2>Join game {state.name}</h2> ++ 
    
      listExistingPlayers ++ 
    
      <h3>Your new player</h3> ++
      ajaxForm(
        <table>
        <tr>
        <td>Your player alias:</td><td>{text(alias, processJoinForm)}</td>
        </tr>
        <tr>
        <td>Trait 1: </td>
        <td>
        {
          selectObj[Trait.Value](Trait.values zip Trait.values.map(_.toString),
                                 Full(traits(0)), 
                                 t => traits = List(t, traits(1)))
        }
        </td>
        </tr>
        <tr>
        <td>Trait 2: </td>
        <td>
        {
          selectObj[Trait.Value](Trait.values zip Trait.values.map(_.toString),
                                 Full(traits(1)), 
                                 t => traits = List(traits(0), t))
        }
        </td>
        </tr>
        <tr>
        <td/>
        <td>
        { ajaxButton("Randomize", () => { 
          reRender
          JsCmds.Noop
        }) }
        <input type="submit" value="Join game" />
        </td>
        </tr>
        </table>
      )
    )
  }
  
  def render = lastState match {
    case None => <p>Loading StarGame data... refresh stuck here</p>
    case Some(state) => if(state.isOneOfThePlayers(user)) {
      renderPlayerView
    } else if(state.started) {
      renderObserverView
    } else {
      if(state.players.length < state.nPlayers)
        renderJoinView
      else
        <p>Sorry, this game is full. Join another or make a new one!</p>
    }
  }
}
