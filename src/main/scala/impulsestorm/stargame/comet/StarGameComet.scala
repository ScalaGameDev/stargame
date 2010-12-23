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

import net.liftweb.actor._
import net.liftweb.common.{Box, Full, Loggable}

import scala.xml._
import scala.util.Random

import impulsestorm.stargame.model._
import impulsestorm.stargame.model.stargame._
import impulsestorm.stargame.lib._

class StarGameComet extends CometActor with Loggable {
  import impulsestorm.stargame.model.StarGame.{supervisor => sg}
  val openid = ImOpenIDVendor.identifier
  val stateId = S.param("gameId").get 
  
  var stateOpt: Option[StarGameState] = None
  var playerOpt: Option[Player] = None
  def state = stateOpt.get
  def player = playerOpt.get
  
  implicit val formats = EnumSerializer.formats
  
  override def autoIncludeJsonCode = true
  
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
    case NoSuchGame => {
      this.error("No such game")
      reRender
    }
    case x => 
      logger.error("StarGameComet unknown message: %s".format(x.toString))
  }
  
  override def receiveJson = {
    case x if {println("Got Json: "+x); false} => Noop 
    case JObject(List(JField("command", JString("researchAllocSave")), 
                      JField("params", allocs))) => {
      val allocation = allocs.values.asInstanceOf[List[Any]].map( _ match {
        case x: Double => x
        case x: BigInt => x.toDouble
      })
      sg ! Actions.ResearchAllocation(this, allocation)
      Noop
    }
    case JObject(List(JField("command", JString("dispatchFleet")), 
                      JField("params", JArray(List(
                        JString(fleetUuid), quantities, JInt(toStarId) 
                      ))))) => {
      val quantitiesValues = 
        quantities.values.asInstanceOf[List[BigInt]].map(_.toInt)      
      
      sg ! Actions.DispatchFleet(this, fleetUuid, quantitiesValues, 
                                 toStarId.intValue)
      Noop
    }
  } 
  
  def viewPlayer = (setTitle("Player view") & setHtmlPlayersList & 
    setHtmlResearch & setHtmlMapCmds & 
    showPanes(List("playersList", "research", "map")) &
    setMyPlayer & setMapView)
    
  def viewObserver =
    setTitle("Game '%s' in progress".format(state.name)) & setHtmlPlayersList &
      showPane("playerList")
  
  def viewJoin =
    setTitle("Join game") & setHtmlPlayersList & setHtmlJoin &
      showPanes(List("playersList", "join"))
  
  def render = {
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
    OnLoad(JsRaw(cmd.mkString("\n")))
  }
  
  def showPane(pane: String) = showPanes(List(pane))
  
  def setTitle(title: String) = OnLoad(SetHtml("title", <h1>{title}</h1>))
  
  private def jsonFuncCmd(functionName: String, obj: Any) =
    OnLoad(Call(
      functionName, Str(compact(JsonAST.render(Extraction.decompose(obj))))))
  
  def setMyPlayer: JsCmd = jsonFuncCmd("setMyPlayer", PlayerInfo.from(player))
  
  def setMapView: JsCmd = jsonFuncCmd("setMapView", MapView.from(state, player))
  
  def setHtmlPlayersList : JsCmd = {  
    def startGame() = {
      sg ! Actions.StartGame(this)
      Noop
    }
    
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
      
    
    OnLoad(SetHtml("playersList", existingPlayersHtml ++ startGameHtml))
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
    def changeResearchChoice(newChoice: Tech) = {
      println("New choice: " + newChoice.longName)
      sg ! Actions.ResearchChoice(this, newChoice)
      Noop
    }

    JsRaw("function jsonResearchAllocSave() {" + 
      jsonSend("researchAllocSave", Call("getSliderVals")).toJsCmd +
      "}") &
    OnLoad(SetHtml("research", (<h2>Research</h2> ++
        <table>
        <tr>
        <td>
        <div id="research-techs">
          <h3>Technologies</h3>
          <ul>
          {
            val techTuple = List(TechCategory.values, player.organizedTechs,
                                 player.canResearchTechs,
                                 player.researchChoices).transpose
            
            (techTuple.map {
              case List(category: TechCategory, 
                        techs: List[Tech], 
                        choices: List[Tech], 
                        currentChoice: Tech) => 
                <li>{category}
                  <ul>
                  {techs.map(t => 
                    <li><span class="tech-complete">{t}</span></li>
                  )}
                  <li>Research choice:
                  {val choices3 = choices.take(3)
                    ajaxSelectObj[Tech](choices3 zip choices3.map(_.longName),
                                        Full(currentChoice), 
                                        changeResearchChoice _)}
                  </li>
                  </ul>
                </li>
            })
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
    )) & 
    (
      Call("setupSliders", JsVar("jsonResearchAllocSave")) &
      Call("setSliderVals", JsArray(player.researchAlloc.map(Num(_)) : _*))
    ))
    
  }
  
  def setHtmlMapCmds = {
    JsRaw("function jsonDispatchFleet(fleetUuid, quantities, toStarId) {" + 
      jsonSend("dispatchFleet", 
        JsRaw("[fleetUuid, quantities, toStarId]")).toJsCmd +
    "}")
  }
}
