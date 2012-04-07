package impulsestorm.stargame.comet

import net.liftweb._
import http._
import http.js._
import http.js.JE._
import http.js.JsCmds._
import json.JsonAST
import json.Printer._
import json.JsonAST._ 
import json.Extraction
import http.SHtml._

import java.util.TimerTask

import net.liftweb.actor._
import net.liftweb.common.{Box, Full, Loggable}

import scala.xml._
import scala.util.Random

import impulsestorm.stargame.model._
import impulsestorm.stargame.model._
import impulsestorm.stargame.lib._

class StarGameComet extends CometActor with Loggable {
  import impulsestorm.stargame.model.StarGame.{supervisor => sg}
  val openid = ImOpenIDVendor.identifier
  val stateId = S.param("gameId").get 
  
  var stateOpt: Option[StarGameState] = None
  var playerOpt: Option[Player] = None
  
  var hintOpt: Option[Hint] = None
  
  var intervalTask: Option[TimerTask] = None
  
  // Tech and Category index
  var openResearchTech: Option[Tech] = None
  
  def state = stateOpt.get
  def player = playerOpt.get
  
  implicit val formats = EnumSerializer.formats
  
  override def autoIncludeJsonCode = true
  
  override def defaultPrefix = Full("comet")
  
  def acceptNewState(state: StarGameState) = {
    stateOpt  = Some(state)
    playerOpt = state.players.find(_.openid == Some(openid))
    
    // schedule task to poll as often as the tick length
    if(state.started && !state.finished && intervalTask.isEmpty) {
      def taskF() = sg ! InquireMapUpdates(stateId, this)
      val interval = math.max(
        (StarGameState.tickSizeYears/state.yearsPerDay*86400*1000).toLong,
        1000)
        
      intervalTask = Some(ImTimer.addTask(taskF, interval, interval))
    }
  }
  
  override def localSetup() = {
    sg ! Subscribe(stateId, this)
    sg ! Inquire(stateId, this)
  }
  
  override def localShutdown() = {
    sg ! Unsubscribe(stateId, this)
    
    // cancel timed task
    intervalTask match {
      case Some(t) => t.cancel
      case _ => Unit
    }
  }
  
  override def lowPriority = {
    case state: StarGameState => {
      logger.info("StarGameComet gets unhinted state")
      acceptNewState(state)
      reRender
    }
    case (state: StarGameState, None) => {
      logger.info("StarGameComet gets unhinted state")
      acceptNewState(state)
      reRender
    }
    case (state: StarGameState, hint: Hint) => {
      logger.info("StarGameComet gets hinted state")
      hintOpt = Some(hint)
      acceptNewState(state)
      if(!state.finished && 
         state.isOneOfThePlayers(openid) && 
         hint.mapInfoChangeOnly)
        partialUpdate(sendHint() & setMapView & setHtmlResearch)
      else
        reRender
    }
    case Actions.ActionError(s) =>
      this.error(s)
    case NoSuchGame => {
      this.error("No such game")
      reRender
    }
    case x => 
      logger.error("StarGameComet unknown message: %s".format(x.toString))
  }
  
  override def receiveJson = {
    case x if {println("Got Json: "+x); false} => Noop
    case JObject(List(JField("command", JString("dispatchShips")), 
                      JField("params", JArray(List(
                        JInt(fromStarId), JInt(quantity), JInt(toStarId) 
                      ))))) => {
      sg ! Actions.DispatchShips(this, fromStarId.intValue, quantity.intValue, 
                                 toStarId.intValue)
      Noop
    }
  } 
  
  def sendHint() = hintOpt match {
    case Some(hint) if !hint.selectedUuid.isEmpty => {
      hintOpt = None
      OnLoad(Call("takeHint", hint.selectedUuid ))
    }
    case _ => Noop    
  }
  
  def viewPlayer(owner: Boolean) = {
    val title = 
      if(state.finished) {
        val victor = state.players(state.gameVictor)
        "Game over. Victory to: %s (%s)".format(
          victor.alias, victor.openid.getOrElse("AI"))
      } else if(state.started)
        "Game in progress"
      else
        "Waiting for players. (%d/%d)".format(
          state.players.length, state.nPlayers)
    
    (setTitle(title) & setHtmlPlayersList & 
    (if(owner) setHtmlStartGame else Noop) & 
    setHtmlResearch & setHtmlMapCmds & 
    showPanes(List("research", "map", "playersList", "startGame", "reports")) &
    setMapView & sendHint())
  }
    
  def viewObserver =
    setTitle("Game '%s' in progress".format(state.name)) & setHtmlPlayersList &
      showPane("playerList")
  
  def viewJoin(owner: Boolean) =
    setTitle("Join game") & setHtmlPlayersList & setHtmlJoin & 
      (if(owner) setHtmlStartGame else Noop) &
      showPanes(List("playersList", "join", "startGame"))
  
  def render = {
    stateOpt match {
      case None => {
        SetHtml("title", <p>Loading state...</p>)
      }
      case Some(state) =>
        val isOwner = openid == state.createdBy
        if(state.finished) {
          viewPlayer(isOwner)
        } else if(state.isOneOfThePlayers(openid)) {
          viewPlayer(isOwner)
        } else if(state.started) {
          viewObserver
        } else {
          viewJoin(isOwner)
        }
    }
  }
  
  def showPanes(panes: List[String]) = {
    val cmd = "$('.pane').hide();" :: panes.map("$('#" + _ + "').show();") 
    OnLoad(JsRaw(cmd.mkString("\n")))
  }
  
  def showPane(pane: String) = showPanes(List(pane))
  
  def setTitle(title: String) = OnLoad(SetHtml("title", <h1>{title}</h1>))
  
  private def jsonFuncCmd(functionName: String, obj: Any) =
    OnLoad(Call(
      functionName, Str(compact(JsonAST.render(Extraction.decompose(obj))))))
  
  def setMapView: JsCmd = // includes player info 
    jsonFuncCmd("setMapView", MapView.from(state, player))
  
  def setHtmlPlayersList : JsCmd = {  
    val players = state.players
    
    val existingPlayersHtml : NodeSeq = ( 
      <h2>Existing players</h2>
      <table>
        {if(players.isEmpty)
           <tr><td /><td>None</td></tr>
         else
           players.map( p => <tr><td></td><td>{p.alias}</td></tr> )}
      </table>
    ) 
    
    OnLoad(SetHtml("playersList", existingPlayersHtml))
  }
  
  def setHtmlStartGame : JsCmd = {
    def startGame() = {
      sg ! Actions.StartGame(this)
      Noop
    }
    val startGameHtml : NodeSeq = 
      if(!state.started && openid == state.createdBy)
        <h2>Start game</h2>
        <p>
        {"%d out of %d slots filled".format(
          state.players.length, state.nPlayers)}
          <br/>
          {ajaxButton("Start game filling slots with AI", startGame _)}
        </p>
      else <div/>
    OnLoad(SetHtml("startGame", startGameHtml))
  }
  
  def setHtmlJoin : JsCmd = {
    var newPlayer: PlayerSpec = PlayerSpec.randomPlayer(Some(openid), state)
    
    def setAlias(a: String) =
      newPlayer = newPlayer.copy(alias=a)
    
    def setTrait(index: Int, t: Trait) =
      newPlayer = newPlayer.copy(traits=newPlayer.traits.updated(index, t))
      
                                   
    def processJoinForm() = {
      logger.info("Registered : " + newPlayer.alias)
      sg ! Actions.RegisterPlayer(this, newPlayer)
      Noop
    }

    val joinHtml = if(state.players.length < state.nPlayers)
      <h3>Your new player</h3> ++
      ajaxForm(
        <table>
        <tr>
        <td>Your player alias:</td>
        <td>{text(newPlayer.alias, a => setAlias _)}</td>
        </tr>
        <tr>
        <td>Trait 1: </td>
        <td>
        {
          selectObj[Trait](Trait.values zip Trait.values.map(_.name),
                           Full(newPlayer.traits(0)), 
                           t => setTrait(0, t))
        }
        </td>
        </tr>
        <tr>
        <td>Trait 2: </td>
        <td>
        {
          selectObj[Trait](Trait.values zip Trait.values.map(_.name),
                           Full(newPlayer.traits(1)), 
                           t => setTrait(1, t))
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
    
    OnLoad(SetHtml("join", joinHtml))
  }
  
  def setHtmlResearch : JsCmd = {
    def buyTech(tech: Tech) : JsCmd= {
      sg ! Actions.BuyTech(this, tech)
      Noop
    }
    
    def techPaneContents(tech: Tech) = 
      <div>
        <h4>{tech.toString}</h4>
        <p>{tech.description}</p>
        <p>{
          if(player.techs.contains(tech)) "Already Researched" 
          else if(player.gold > tech.cost) 
            ajaxButton("Buy - " + tech.cost + "RU", () => buyTech(tech))
          else
            "Cannot afford."
        }</p>
      </div>
    
    def showTechPane(tech: Tech) = () => {
      openResearchTech = Some(tech)
      SetHtml("research-tech-info", techPaneContents(tech))
    }
    
    def styleTech(tech: Tech) = {
      if(player.techs.contains(tech)) {
        <span>{tech.toString} - Done</span>
      } else {
        <span>{tech.toString} - {tech.cost} RU</span>
      }
    }

    OnLoad(SetHtml("research", (<h2>Research</h2> ++
        <table>
        <tr>
        <td width="50%">
          <h3>Technologies</h3>
          <div id="research-accordian">
          {
            List( TechCategory.values,
                  Tech.allOrganizedTechs ).transpose map { 
              case List(category: TechCategory, categoryTechs: List[Tech]) =>
                <h4 style="padding-left: 30px;">{category}</h4> ++
                <div>
                  <ul>
                  {
                    categoryTechs.map(t =>
                      <li>{a(showTechPane(t), styleTech(t))}</li>
                    )
                  }
                  </ul>
                </div>
            }
          }
          </div>
        </td>
        <td>
        <div id="research-tech-info">
        {
          openResearchTech match {
            case Some(tech) => techPaneContents(tech)
            case None => <div/>
          }
        }
        </div>
        </td>
        </tr>
        </table>
    )) & (openResearchTech match {
      case None => Noop
      case Some(tech) => showTechPane(tech)()
    })) & JsRaw("""
    $('#research-accordian').accordion({
      active: lastResearchCategory
    });
    $('#research-accordian').bind('accordionchangestart', function(event, ui) {
      lastResearchCategory = $('#research-accordian').accordion( "option", "active" );
    });
    """)
    
  }
  
  def setHtmlMapCmds = {
    JsRaw("function jsonDispatchShips(fromStarId, quantity, toStarId) {" + 
      jsonSend("dispatchShips", 
        JsRaw("[fromStarId, quantity, toStarId]")).toJsCmd +
    "}")
  }
}
