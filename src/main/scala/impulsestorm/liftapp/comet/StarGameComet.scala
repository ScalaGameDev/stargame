package impulsestorm.liftapp.comet

import net.liftweb._
import http._
import http.js._
import http.js.JE._
import http.js.JsCmds._
import http.SHtml._

import net.liftweb.actor._
import net.liftweb.common.{Box, Full, Loggable}

import scala.xml._
import scala.util.Random

import impulsestorm.liftapp.model._
import impulsestorm.liftapp.model.stargame._
import impulsestorm.liftapp.lib._

class StarGameComet extends CometActor with Loggable {
  import impulsestorm.liftapp.model.StarGame.{supervisor => sg}
  val openid = ImOpenIDVendor.identifier
  val stateId = S.param("gameId").get 
  
  var stateOpt: Option[StarGameState] = None
  var playerOpt: Option[Player] = None
  def state = stateOpt.get
  def player = playerOpt.get
  
  override def defaultPrefix = Full("comet")
  
  def acceptNewState(state: StarGameState) {
    stateOpt  = Some(state)
    playerOpt = state.players.find(_.openid == Some(openid))
  }
  
  override def localSetup() = {
    sg ! Subscribe(stateId, this)
    sg ! Inquire(stateId, this)
  }
  
  override def localShutdown() = {
    sg ! Unsubscribe(stateId, this)
  }
  
  override def lowPriority = {
    case state: StarGameState => {
      logger.info("StarGameComet gets unhinted state")
      acceptNewState(state)
      reRender
    }
    case Actions.ActionError(s) =>
      this.error(s)
    case NoSuchGame =>
    {
      this.error("No such game")
    }
    case x => 
      logger.error("StarGameComet unknown message: %s".format(x.toString))
  }
  
  def viewPlayer =
    setTitle("Player view") & setHtmlPlayersList & setHtmlResearch &
      showPanes(List("playersList", "research"))
  
  def viewObserver =
    setTitle("Game '%s' in progress".format(state.name)) & setHtmlPlayersList &
      showPane("playerList")
  
  def viewJoin =
    setTitle("Join game") & setHtmlPlayersList & setHtmlJoin &
      showPanes(List("playersList", "join"))
  
  def render = {
    logger.info("Call to render")
    stateOpt match {
      case None => {
        <p>Loading state...</p>
      }
      case Some(state) => if(state.isOneOfThePlayers(openid)) {
        viewPlayer
      } else if(state.started) {
        viewObserver
      } else {
        viewJoin
      }
    }
  }
  
  def showPanes(panes: List[String]) = {
    val cmd = "$('.pane').hide();" :: panes.map("$('#" + _ + "').show();") 
    JsRaw(cmd.mkString("\n"))
  }
  
  def showPane(pane: String) = showPanes(List(pane))
  
  def setTitle(title: String) = SetHtml("title", <h1>{title}</h1>)
  
  def setHtmlPlayersList : JsCmd = {
    val players = state.players
    SetHtml("playersList",
      <h2>Existing players</h2>
      <table>
        {
          if(players.isEmpty)
            <tr><td /><td>None</td></tr>
          else
            players.map( p => <tr><td></td><td>{p.alias}</td></tr> )
        }
      </table>
    )
  }
  
  def setHtmlJoin : JsCmd = {
    var alias : String = 
      SimRandom.randomNoCollisions(StarGameState.aliases, 
                                   state.players.map(_.alias))
    
    var traits = Random.shuffle(Trait.values) take 2
                                   
    def processJoinForm() = {
      logger.info("Registered : " + alias)
      sg ! Actions.RegisterPlayer(stateId, this, Full(openid), alias, traits)
      JsCmds.Noop
    }

    val joinHtml = if(state.players.length < state.nPlayers)
      <h3>Your new player</h3> ++
      ajaxForm(
        <table>
        <tr>
        <td>Your player alias:</td><td>{text(alias, alias = _)}</td>
        </tr>
        <tr>
        <td>Trait 1: </td>
        <td>
        {
          selectObj[Trait](Trait.values zip Trait.values.map(_.name),
                           Full(traits(0)), 
                           t => traits = List(t, traits(1)))
        }
        </td>
        </tr>
        <tr>
        <td>Trait 2: </td>
        <td>
        {
          selectObj[Trait](Trait.values zip Trait.values.map(_.name),
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
        { ajaxSubmit("Join game", processJoinForm _) }
        </td>
        </tr>
        </table>
      )
    else
      <p>Sorry, this game is full. Join another or make a new one!</p>
    
    SetHtml("join", joinHtml)
  }
  
  def setHtmlResearch : JsCmd = {
    def adjustAlloc(allocationIndex: Int, newAllocation: String) = {
      println("New allocation %s - %s".format(allocationIndex, newAllocation))
      JsCmds.Noop
    }
    
    SetHtml("research", (<h2>Research</h2> ++
        <table>
        <tr>
        <td>
        <div id="researched-techs">
          <h3>Researched technologies</h3>
          <ul>
          {
            (TechCategory.values zip player.organizedTechs).map {
              case(category, techs) => 
                <li>{category}
                  <ul>
                  {techs.map(t => <li>{t}</li>)}
                  </ul>
                </li>
            }
          }
          </ul>
        </div>
        </td>
        <td>
        <div id="research-allocation">
        {
          (0 until TechCategory.values.length).map(i => {
            <p>{TechCategory.values(i)}
              <br/>
              <div id={"research-slider-"+i.toString} 
                class="research-slider" />
            </p>
          })
        }
        
        </div>
        </td>
        </tr>
        </table>
        <button onclick="techSaveClicked()">Save</button>
    )) & 
    JsRaw("function techSaveClicked() {" + 
        jsonSend("research-alloc-save", Call("getSliderVals")).toJsCmd +
        "}") &
    (
      Call("setupSliders") &
      Call("setSliderVals", JsArray(player.researchAlloc.map(Num(_)) : _*))
    )
    
  }
}
