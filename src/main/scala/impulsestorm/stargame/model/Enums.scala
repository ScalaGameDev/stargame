package impulsestorm.stargame.model

import scala.util.Random
import impulsestorm.stargame.lib.SimRandom._

import net.liftweb.json._

trait hasName {
  val name: String
  override def toString = name
}

trait Enumerator[EnumeratedT <: hasName] {
  val eclass : Class[EnumeratedT]
  
  def withName(name: String) = vmap(name)
  
  def addToMap(value: EnumeratedT) = {
    vmap = vmap + (value.name->value)
    values = values :+ value
    value
  }
  
  var vmap : Map[String, EnumeratedT] = Map()
  var values : List[EnumeratedT] = Nil
}

class EnumSerializer[EnumeratedT <: hasName]
  (val enum: Enumerator[EnumeratedT])
  extends Serializer[EnumeratedT] {

  import JsonDSL._
  
  def deserialize(implicit format: Formats):
    PartialFunction[(TypeInfo, JValue), EnumeratedT] = {
    case (TypeInfo(x, z), json) if x==enum.eclass => json match {
      case JString(value) => enum.withName(value)
      case value => 
        throw new MappingException("Can't convert " +
                                   value + " to "+ enum.eclass)
    }
  }
  
  def serialize(implicit format: Formats): PartialFunction[Any,JValue] = {
    case i: EnumeratedT => i.name
  }
}

object EnumSerializer {
  val li = 
    List(TechCategory, 
         Tech, 
         Trait, 
         StarClass, 
         PlanetZone, 
         PlanetType).map(new EnumSerializer(_))
  
  val formats : Formats = li.foldLeft(Serialization.formats(NoTypeHints))(_+_)
}

case class TechCategory(name: String) extends hasName

object TechCategory extends Enumerator[TechCategory] {
  // 0 - propulsion. Ship warp speed and manuverability
  val Propulsion = Value("Propulsion") // 4
  // 1 - Weapons - Beam, projectile, missiles
  val Combat     = Value("Combat")
  // 2 - Setttling new planet types, max population, infrastructure cost
  val Civil      = Value("Civil")
  // 3 - Productivity and allegiance
  val Policy     = Value("Policy")

  def organizeTechs(techs: List[Tech]) = values.map { cat =>  
    techs.filter(_.category==cat).sortBy(_.level)
  }
  
  val eclass = classOf[TechCategory]
  private def Value(name: String) =
    addToMap(TechCategory(name))
  
  val defaultAllocation : List[Double] = List.fill(6)(1.0/6)
}

case class Tech(name: String, longName: String, category: TechCategory, 
                level: Int,
                description: String) 
  extends hasName
{
  override def toString = longName
  
  lazy val cost = (40 * math.exp(0.15*level)).toInt * 10
  
}

object Tech extends Enumerator[Tech] {
  import TechCategory.{Propulsion, Combat, Civil, Policy}
  
  val Range1        = Value("Range4", "Hydrogen fuel cells", Propulsion,
                            2, "Ships have a range of 6")
  val Range2        = Value("Range5", "Deuterium fuel cells", Propulsion,
                            5, "Ships have a range of 7")
  val Engines2      = Value("Engines2", "Ion engines", Propulsion,
                            6, "Ships move at warp speed 2")
  val Range3        = Value("Range6", "Tritium fuel cells", Propulsion,
                            9, "Ships have a range of 8")
  val Engines3      = Value("Engines3", "Nuclear engines", Propulsion,
                            12, "Equipped ships move at warp speed 3")
  val Range4        = Value("Range7", "Irridium fuel cells", Propulsion,
                            14, "Ships have a range of 9")
  val Engines4      = Value("Engines4", "Fusion engines", Propulsion,
                            18, "Equipped ships mave at warp speed 4")
  val Range5        = Value("Range8", "Uridium fuel cells", Propulsion,
                            19, "Ships have a range of 10")
  val Range6        = Value("Range9", "Reajax II fuel cells", Propulsion,
                            23, "Ships have a range of 11")
  val Engines5      = Value("Engines5", "Antimatter engines", Propulsion,
                            24, "Equipped ships move at warp speed 5")
  val Range7        = Value("Range10", "Antimatter fuel cells", Propulsion,
                            29, "Ships have a range of 12")
  val Engines6      = Value("Engines6", "Subspace engines", Propulsion,
                            30, "Eqipped ships move at warp speed 6")
                                
  //---------------------------------------------------------------------
  val Combat1       = Value("Combat1", "Enhanced weaponry", Combat,
                            2, "Sets battle rating to 110")
  val Combat2       = Value("Combat2", "Tempered armor", Combat,
                            5, "Sets battle rating to 125")
  val Combat3       = Value("Combat1", "Enhanced weaponry", Combat,
                            8, "Sets battle rating to 150")
  val Combat4       = Value("Combat2", "Tempered armor", Combat,
                            13, "Sets battle rating to 170")
  val Combat5       = Value("Combat1", "Enhanced weaponry", Combat,
                            17, "Sets battle rating to 200")
  val Combat6       = Value("Combat2", "Tempered armor", Combat,
                            21, "Sets battle rating to 250")
  val Combat7       = Value("Combat1", "Enhanced weaponry", Combat,
                            25, "Sets battle rating to 300")
  val Combat8       = Value("Combat2", "Tempered armor", Combat,
                            30, "Sets battle rating to 360")         
  val Scanner1 = Value("Scanner1", "Deep space scanner", Combat,
                       5, "Colonies can detect ships 5 ly away")                 
  val Scanner2 = Value("Scanner2", "Improved space scanner", Combat,
                       14, "Colonies can detect ships 7 ly away")
  val Scanner3 = Value("Scanner3", "Advanced space scanner", Combat,
                       21, "Colonies can detect ships 11 ly away")
  val Scanner4 = Value("Scanner4", "Subspace scanner", Combat,
                       30, "Colonies can detect ships 13 ly away")
  
  //---------------------------------------------------------------------
  // Civil - new planet types, max population, infrastructure cost, maintainence
  val MaxPop10    = Value("MaxPop10", "Coordinated zoning", Civil,
                          4, "Max population +10%")
  val IceCol      = Value("IceCol", "Arctic exploration", Civil,
                          6, "Can colonize Ice planets")
  val BarrenCol   = Value("BarrenCol", "Barren colonization", Civil,
                          9, "Can colonize Barren planets")
  val MaxPop20    = Value("MaxPop20", "Competent planning", Civil,
                          11, "Max population +20%")
  val DeadCol     = Value("DeadCol", "Ecodomes", Civil,
                          13, "Can colonize Dead planets")
  val AsteroidCol = Value("AsteroidCol", "Microgravity survival", Civil,
                          14, "Can colonize Asteroids")
  val InfernoCol  = Value("InfernoCol", "Tectonic stabilization", Civil,
                          17, "Can colonize Inferno planets")
  val HostilePop  = Value("HostilePop", "Hostile environmental control", Civil,
                          18, "Ice/Barren/Dead/Inferno planets: Normal growth")
  val GasGiantCol = Value("GasGiantColonies", "Atmospheric colonization", Civil,
                          20, "Can colonize Gas Giants")
  val MaxPop30    = Value("MaxPop30", "Dense zoning", Civil,
                          24, "Max population +30%")
 
  // Policy - productivity and allegiance
  val Grow1   = Value("Grow1", "Colonial medicine", Policy,
                      3, "Population growth rate +20%")
  val Prod2   = Value("Prod2", "Competent industrial management", Policy,
                      8, "140% shipyard productivity")
  val Grow2   = Value("Grow2", "Incentivized colonization", Policy,
                      12, "Population growth rate +40%")
  val Prod3   = Value("Prod3", "Operations research", Policy,
                      15, "220% shipyard productivity")
  val Grow3   = Value("Grow3", "Abundant opportunity", Policy,
                      18, "Population growth rate +70%")
  val Prod4   = Value("Prod4", "Lean manufacturing", Policy,
                      21, "400% factory productivity")
  val Grow4   = Value("Grow4", "Colonial self-sufficiency", Policy,
                      25, "Population growth rate +100%")
  val Prod5   = Value("Prod5", "Complete automation", Policy,
                      28, "600% shipyard productivity")
  
  val allOrganizedTechs = TechCategory.organizeTechs(values)
                      
  val eclass = classOf[Tech]
  
  private def Value(name: String, longName: String, category: TechCategory,
                    level: Int, description: String) =
    addToMap(Tech(name, longName, category, level, description))
}

case class Trait(name: String) extends hasName

object Trait extends Enumerator[Trait] {
  val Militaristic  = Value("Militaristic")
  val Scientific    = Value("Scientific")
  val Expansionist  = Value("Expansionist")
  val Organized     = Value("Organized")
  val Industrious   = Value("Industrious")
  val Diplomatic    = Value("Diplomatic")
  val Commercial    = Value("Commercial")

  val eclass = classOf[Trait]
  private def Value(name: String) =
    addToMap(Trait(name))
}

case class StarClass(name: String) extends hasName

object StarClass extends Enumerator[StarClass] {
  val Giant   = Value("Giant")
  val B       = Value("B")
  val A       = Value("A")
  val F       = Value("F")
  val G       = Value("G")
  val K       = Value("K")
  val M       = Value("M")
  val Compact = Value("Compact")

  val eclass = classOf[StarClass]
  private def Value(name: String) =
    addToMap(StarClass(name))
  
  val abundances = 
    List( 0.03->Giant,
          0.03->B,
          0.05->A,
          0.10->F,
          0.12->G,
          0.12->K,
          0.40->M,
          0.19->Compact )
  
  def randomNPlanets(sClass: StarClass) = {
    val (minPlanets, maxPlanets) = sClass match {
      case Giant    => (1,3)
      case B        => (1,5)
      case A        => (1,5)
      case F        => (2,8)
      case G        => (2,8)
      case K        => (1,7)
      case M        => (1,3)
      case Compact  => (0,1)
    }
    
    random(minPlanets, maxPlanets)
  }
  
  def randomZone(sClass: StarClass, totalPlanets: Int) =
    if      ( List(Giant, B, A, M) contains sClass )
      randomObj(PlanetZone.values)
    else if ( List(F, G, K)        contains sClass )
      // shift probability towards outer for 6 or more planets
      weightedRandom( 
        List( math.min(1/3.0, 0.5-totalPlanets/32.0)->PlanetZone.Inner,
              math.min(1/3.0, 0.5-totalPlanets/32.0)->PlanetZone.Middle,
              math.max(1/3.0, totalPlanets/16.0)    ->PlanetZone.Outer ) )
    else PlanetZone.Outer
    
    
}

case class PlanetZone(name: String, order: Int) extends hasName

object PlanetZone extends Enumerator[PlanetZone] {
  val Inner  = Value("Inner", 1)
  val Middle = Value("Middle", 2)
  val Outer  = Value("Outer", 3)
  
  val eclass = classOf[PlanetZone]
  private def Value(name: String, order: Int) =
    addToMap(PlanetZone(name, order))
  
  def randomPType(sClass: StarClass, 
                  zone: PlanetZone) : PlanetType = {
    import PlanetType._
    import StarClass._
    zone match {
      case Inner => 
        weightedRandom(List( 0.05->Asteroid,
                             0.05->GasGiant,
                             0.50->Dead,
                             0.10->Barren,
                             0.30->Inferno ))
      case Middle => 
        if(List(F,G,K) contains sClass)
          weightedRandom(List( 0.10->Asteroid,
                               0.05->GasGiant,
                               0.10->Dead,
                               0.20->Barren,
                               0.10->Ice,
                               0.15->Inferno,
                               0.10->Terran,
                               0.10->Ocean,
                               0.10->Arid ))
         else
          weightedRandom(List( 0.10->Asteroid,
                               0.05->GasGiant,
                               0.10->Dead,
                               0.20->Barren,
                               0.10->Ice,
                               0.15->Inferno,
                               0.05->Terran,
                               0.05->Ocean,
                               0.05->Arid,
                               0.05->Barren,
                               0.05->Ice,
                               0.05->Dead ))
      case Outer =>
        weightedRandom(List( 0.15->Asteroid,
                             0.60->GasGiant,
                             0.05->Ice,
                             0.05->Barren,
                             0.10->Dead ))
                         
    }
  }
}

case class PlanetType(name: String, requisiteTech: Option[Tech]) 
extends hasName

object PlanetType extends Enumerator[PlanetType] {
  import Tech._
  
  val Terran    = Value("Terran")
  val Ocean     = Value("Ocean")
  val Arid      = Value("Arid")
  val Ice       = Value("Ice", Some(IceCol))
  val Barren    = Value("Barren", Some(BarrenCol))
  val Dead      = Value("Dead", Some(DeadCol))
  val Inferno   = Value("Inferno", Some(InfernoCol))
  val Asteroid  = Value("Asteroid", Some(AsteroidCol))
  val GasGiant  = Value("Gas Giant", Some(GasGiantCol))
  
  val eclass = classOf[PlanetType]
  private def Value(name: String, requisiteTech: Option[Tech] = None) =
    addToMap(PlanetType(name, requisiteTech))
  
  def randomBaseMaxPop(pType: PlanetType) = pType match {
    case Terran   => random(60.0, 130.0)
    case Ocean    => random(30.0, 70.0)
    case Arid     => random(20.0, 50.0)
    case Ice      => random(15.0, 25.0)
    case Barren   => random(5.0,  15.0)
    case Dead     => random(3.0,  7.0)
    case Inferno  => random(2.0,  5.0)
    case Asteroid => random(1.0,  3.0)
    case GasGiant => random(10.0, 30.0)
  }
  
  def randomMineralWealth(pType: PlanetType) = pType match {
    case Terran   => random(0.8,  1.2)
    case Ocean    => random(0.2,  1.0)
    case Arid     => random(0.8,  2.5)
    case Ice      => random(0.8,  1.5)
    case Barren   => random(1.2,  2.5)
    case Dead     => random(1.2,  3.0)
    case Inferno  => random(2.0,  4.0)
    case Asteroid => random(3.0,  6.0)
    case GasGiant => random(0.0,  0.2)
  }
}
