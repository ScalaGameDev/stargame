package impulsestorm.stargame.model.stargame

import impulsestorm.stargame.lib._

import scala.util.Random

import org.bson.types.ObjectId

case class Player( id: Int, openid: Option[String], alias: String,
                   traits: List[Trait],
                   exploredStarIds: List[Int],
                   metPlayerIds: List[Int],
                   designs: List[Design],
                   gold: Double,
                   techs: List[Tech], 
                   researchAlloc: List[Double],
                   researchChoices: List[Tech],
                   canResearchTechs: List[List[Tech]])
{
  lazy val organizedTechs = TechCategory.organizeTechs(techs)
  
  lazy val sensorRange : Int = {
    val sensorTechs = organizedTechs(3)
    if(sensorTechs.contains(Tech.Scanner4))
      11
    else if(sensorTechs.contains(Tech.Scanner3))
      7
    else if(sensorTechs.contains(Tech.Scanner2))
      5
    else if(sensorTechs.contains(Tech.Scanner1))
      3
    else
      1
  }
  
  lazy val shipRange : Int = {
    val pTechs = organizedTechs(0)
    if(pTechs.contains(Tech.Range7))
      12
    else if(pTechs.contains(Tech.Range6))
      11
    else if(pTechs.contains(Tech.Range5))
      10
    else if(pTechs.contains(Tech.Range4))
      9
    else if(pTechs.contains(Tech.Range3))
      8
    else if(pTechs.contains(Tech.Range2))
      7
    else if(pTechs.contains(Tech.Range1))
      6
    else
      5
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
  
  def startingPlayer( id: Int, pSpec: PlayerSpec, startingStarId: Int) = {
    val canResearchTechs = Tech.generateCanResearchTechs()
    Player(id, pSpec.openid, pSpec.alias, pSpec.traits, 
           exploredStarIds = List(startingStarId),
           metPlayerIds = List(id),
           gold = 0, designs = Design.startingDesigns, 
           techs = Tech.startingTechs, 
           researchAlloc=TechCategory.defaultAllocation,
           researchChoices=canResearchTechs.map(_.head),
           canResearchTechs=canResearchTechs)
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
