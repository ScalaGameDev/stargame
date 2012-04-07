package impulsestorm.stargame.model

import impulsestorm.stargame.lib._

import scala.util.Random

import org.bson.types.ObjectId

case class Player( id: Int, openid: Option[String], alias: String,
                   traits: Set[Trait],
                   homeStarId: Int,
                   exploredStarIds: Set[Int],
                   metPlayerIds: Set[Int],
                   gold: Double,
                   techs: List[Tech])
{
  
  import Tech._
  
  lazy val sensorRange : Int = {
    if(techs.contains(Scanner4))
      11
    else if(techs.contains(Scanner3))
      7
    else if(techs.contains(Scanner2))
      5
    else if(techs.contains(Scanner1))
      3
    else
      1
  }
  
  lazy val range : Int = {
    if(techs.contains(Range7))
      13
    else if(techs.contains(Range6))
      12
    else if(techs.contains(Range5))
      11
    else if(techs.contains(Range4))
      10
    else if(techs.contains(Range3))
      9
    else if(techs.contains(Range2))
      8
    else if(techs.contains(Range1))
      7
    else
      6
  }
  
  lazy val speed : Double = {
    if(techs.contains(Engines6))
      2.5
    else if(techs.contains(Engines5))
      2.2
    else if(techs.contains(Engines4))
      1.9
    else if(techs.contains(Engines3))
      1.6
    else if(techs.contains(Engines2))
      1.3
    else
      1
  }
  
  lazy val battlePower : Int = {
    if(techs.contains(Combat8))
      360
    else if(techs.contains(Combat7))
      300
    else if(techs.contains(Combat6))
      250
    else if(techs.contains(Combat5))
      200
    else if(techs.contains(Combat4))
      170
    else if(techs.contains(Combat3))
      150
    else if(techs.contains(Combat2))
      125
    else if(techs.contains(Combat1))
      110
    else
      100
  }
  
  lazy val maxPopMultiplier : Double = {
    if(techs.contains(MaxPop30)) 
      1.3
    else if(techs.contains(MaxPop20)) 
      1.2
    else if(techs.contains(MaxPop10)) 
      1.1
    else 
      1.0
  }
}

object Player {
  //http://www.rinkworks.com/namegen/ - Japanese
  val aliases = List("Hoshisaka", "Akisa", "Zenchi", "Uesaki", "Sawa", "Ohira",
                     "Taku", "Ruyu", "Woyu", "Tsuya", "Rumata", "Munen",
                     // Chinese (modified a bit)
                     "Zhao", "Hou", "HianLao", "Shia", "Qao", "ShouWiann", "Mao", "Xian", "Chan", "Chao", "LiBai",
                     // Greek!
                     "Athoios", "Ildeos", "Osuos", "Rilytia", "Phacios",
                     "Queyos", "Gariatia", "Atetia", "Honatia", "Nikios",
                     // Pokemon
                     "Awertle", "Kelortle", "Pinizard", "Wadochu", "Skullezard",
                     // Fantasy - vowels
                     "Iri", "Airalsti", "Yana", "Haisri", "Aedue", "Raerithp",
                     // Other fantasy
                     "Taing", "Hatmor", "Irum", "Perdis", "Roob", "Rildan",
                     "Kesale", "Droshin", "Syd", "Ispertai", "Beper", "Osack"
                     )
  
  def startingPlayer( id: Int, pSpec: PlayerSpec, homeStarId: Int) = {
    Player(id, pSpec.openid, pSpec.alias, pSpec.traits.toSet,
           homeStarId = homeStarId,
           exploredStarIds = Set(homeStarId),
           metPlayerIds = Set(id),
           gold = 1000,
           techs = Nil)
  }
}

case class PlayerSpec(openid: Option[String], alias: String, 
                      traits: List[Trait])

object PlayerSpec {
  def randomPlayer(openid: Option[String], state: StarGameState) = {
    val alias = 
      SimRandom.randomNoCollisions(Player.aliases, 
                                   state.players.map(_.alias))
    
    val traits = Random.shuffle(Trait.values) take 2
    
    PlayerSpec(openid, alias, traits)
  }
  
  def randomAIPlayer(state: StarGameState) = randomPlayer(None, state)
}
