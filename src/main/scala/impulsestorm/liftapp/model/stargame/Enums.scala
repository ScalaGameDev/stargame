package impulsestorm.liftapp.model.stargame

import scala.util.Random
import impulsestorm.liftapp.lib.SimRandom._

import net.liftweb.json._
import net.liftweb.json.JsonAST._

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
         PlanetType, 
         ShipSize,
         Engine,
         Sensor,
         ShipModule).map(new EnumSerializer(_))
  
  val formats : Formats = li.foldLeft(Serialization.formats(NoTypeHints))(_+_)
}

case class TechCategory(name: String) extends hasName

object TechCategory extends Enumerator[TechCategory] {
  // 0 - propulsion. Ship warp speed and manuverability
  val Propulsion = Value("Propulsion") // 4
  // 1 - Weapons - Beam, projectile, missiles
  val Weapons    = Value("Weapons")   // 3  
  // 2 - Protection - Shields and armor 
  val Protection = Value("Protection")
  
  // 3 - Battle computers, Ship detection, ECM
  val Sensors    = Value("Sensors")
  // 4 - Setttling new planet types, max population, infrastructure cost
  val Civil      = Value("Civil")
  // 5 - Productivity and allegiance
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
}

object Tech extends Enumerator[Tech] {
  import TechCategory.{Propulsion, Weapons, Protection, Sensors, Civil, Policy}
  
  val Engines1      = Value("Engines1", "Chemical engines", Propulsion,
                            1, "Equipped ships move at warp speed 1")
  val Range4        = Value("Range4", "Hydrogen fuel cells", Propulsion,
                            2, "Ships have a range of 4")
  val Range5        = Value("Range5", "Deuterium fuel cells", Propulsion,
                            5, "Ships have a range of 5")
  val Engines2      = Value("Engines2", "Ion engines", Propulsion,
                            6, "Ships move at warp speed 2")
  val Range6        = Value("Range6", "Tritium fuel cells", Propulsion,
                            9, "Ships have a range of 6")
  val ModManuver2   = Value("ModManuver2", "Vectored thrust", Propulsion,
                            10, "Add-on: Fighters/corvettes +2 evade")
  val Engines3      = Value("Engines3", "Nuclear engines", Propulsion,
                            12, "Equipped ships move at warp speed 3")
  val Range7        = Value("Range7", "Irridium fuel cells", Propulsion,
                            14, "Ships have a range of 7")
  val FCManuver2    = Value("FCManuver2", "Burst power", Propulsion,
                            16, "All fighters and corvettes: +2 evade")
  val Engines4      = Value("Engines4", "Fusion engines", Propulsion,
                            18, "Equipped ships mave at warp speed 4")
  val Range8        = Value("Range8", "Uridium fuel cells", Propulsion,
                            19, "Ships have a range of 8")
  val ModFrigManuv2 = Value("ModFrigManuv2", "Inertial dampener", Propulsion,
                            20, "Add-on: Frigates/capitals: +2 evade")
  val Range9        = Value("Range9", "Reajax II fuel cells", Propulsion,
                            23, "Ships have a range of 9")
  val Engines5      = Value("Engines5", "Antimatter engines", Propulsion,
                            24, "Equipped ships move at warp speed 5")
  val FrigManuv2    = Value("FrigManuv2", "Captial manuvering", Propulsion,
                            27, "All frigates and capitals: +2 evade")
  val Range10       = Value("Range10", "Antimatter fuel cells", Propulsion,
                            29, "Ships have a range of 10")
  val Engines6      = Value("Engines6", "Subspace engines", Propulsion,
                            30, "Eqipped ships move at warp speed 6")
                                
  //---------------------------------------------------------------------
  val Gun1         = Value("Gun1", "Chaingun", Weapons,
                           1, "2-8 physical damage, -1 to hit")
  val Beam1        = Value("Beam1", "Laser", Weapons,
                           2, "1-4 energy damage")
  val Missile1     = Value("Missile1", "Missiles", Weapons,
                           2, "4 damage to target, +1 to hit")                               
  val Bomb1        = Value("Bomb1", "Planetary bombs", Weapons,
                           3, "3-12 damage to planetary targets only")
  val Missile2     = Value("Missile2", "Thermite Missiles", Weapons,
                           4, "6 damage to target, +1 to hit")
  val BeamMulti1   = Value("BeamMulti1", "Gatling lasers", Weapons,
                           5, "1-4 energy damage, 4x fire rate")
  val GunAntiF     = Value("GunAntiF", "Flak cannon", Weapons,
                           7, "1-4 physical damage, +3 hit fighters/ corvettes")
  val MissileAntiC = Value("MissileAntiC", "Buster rockets", Weapons,
                           8, "10 damage, -1 to hit")
  val Bomb2        = Value("Bomb2", "Fusion bombs", Weapons,
                           9, "4-16 damage to planetary targets only")
  val Beam2        = Value("Beam2", "Capacitor laser", Weapons,
                           10, "3-8 energy damage")
  val MissileScat1 = Value("MissileScat1", "Scatter pack missiles", Weapons,
                           11, "Splits into 5 missiles doing 6 damage each")
  val Gun2         = Value("Gun2", "Coilgun", Weapons,
                           13, "5-11 physical damage")
  val Missile3     = Value("Missile3", "Seeker missiles", Weapons,
                           14, "10 damage, +2 to hit")
  val Beam3        = Value("Beam3", "Proton beam", Weapons,
                           15, "4-10 energy damage")
  val Bomb3        = Value("Bomb3", "Antimatter bombs", Weapons,
                           16, "5-20 damage to planetary targets only")
  val Missile4     = Value("Missile4", "Stringer missiles", Weapons,
                           18, "15 damage, +3 to hit")
  val GunSlow3     = Value("GunSlow3", "Railgun", Weapons,
                           19, "15-30 physical damage, 1/2 fire rate")
  val Beam4        = Value("Beam4", "Neutron beam", Weapons,
                           21, "4-16 energy damage")
  val GunAntiF2    = Value("GunAntiF2", "Heavy flak cannon", Weapons,
                           22, "1-8 physical damage, +3 to hit fighters and corvettes")
  val Bomb4        = Value("Bomb4", "Omega bomb", Weapons,
                           23, "10-50 damage to planetary targets only")
  val Missile5     = Value("Missile5", "Antimatter torpedos", Weapons,
                           24, "30 damage")
  val Gun3         = Value("Gun3", "Helical railgun", Weapons,
                           26, "15-30 physical damage")
  val Missile6     = Value("Missile6", "Smart missiles", Weapons,
                           27, "25 damage, +5 to hit")
  val BeamMulti4   = Value("BeamMulti4", "Autoblaster", Weapons,
                           29, "4-16 energy damage, 3x fire rate")
  val Beam5        = Value("Beam5", "Hard beam", Weapons,
                           30, "6-30 energy damage")
                                 
  //---------------------------------------------------------------------
  val Armor1   = Value("Armor1", "Titanium armor", Protection,
                       1, "A lightweight material for ship hulls")
  val Shields1 = Value("Shields1", "Class I shields", Protection,
                       1, "Reduces energy damage by 1")
  val Shields2 = Value("Shields2", "Class II shields", Protection,
                       4, "Reduces energy damage by 2")
  val Armor2   = Value("Armor2", "Duralloy armor", Protection,
                       8, "Equipping ships have 150% base HP")
  val Shields3 = Value("Shields3", "Class III Shields", Protection,
                       10, "Reduces energy damage by 3, other types by 1")
  val PShiel5  = Value("PShiel5", "Class V Planetary Shields", Protection,
                       12, "Reduces damage taken by planetary targets by 5.")
  val Repair1  = Value("Repair1", "Automated Repair Module", Protection,
                       14, "Add-on: Repairs 15% of remaining HP per turn.")
  val Shields4 = Value("Shields4", "Class IV Shields", Protection,
                       14, "Reduces energy damage by 4, other types by 2")
  val Armor3   = Value("Armor3", "Neutronium armor", Protection,
                       17, "Equipping ships have 200% base HP")
  val Shields5 = Value("Shields5", "Class V Shields", Protection,
                       20, "Reduces energy damage taken by 5, other types by 2")
  val PShiel10 = Value("PShiel10", "Class X Planetary Shields", Protection,
                       22, "Reduces damage taken by planetary targets by 10.")
  val Shields6 = Value("Shields6", "Class VI Shields", Protection,
                       22, "Reduces energy damage taken by 6, other types by 3")
  val Armor4   = Value("Armor4", "Metamaterial armor", Protection,
                       26, "Equipping ships have 250% base HP")
  val Shields7 = Value("Shields7", "Class VII Shields", Protection,
                       27, "Reduces energy damage taken by 7, other types by 4")
  val MisslShd = Value("MisslShd", "Missile Shield", Protection,
                       28, "Add-on: 75% chance of destroying incoming missiles, -1% per missile tech level.")
  val Repair2  = Value("Repair2", "Advanced damage control", Protection,
                       30, "Add-on: Repairs 30% of remaining HP per turn")
  
  //----------------------------------------------------------------------
                                 
  val Sensors1 = Value("Sensors1", "Combat sensors Alpha", Sensors,
                       2, "+1 to hit (excluding missiles)")
  val ECM1     = Value("ECM1", "ECM Alpha", Sensors,
                       3, "Add-on: +1 evade missiles")
  val Scanner1 = Value("Scanner1", "Deep space scanner", Sensors,
                       5, "Colonies can detect ships 3 ly away")
  val Sensors2 = Value("Sensors2", "Combat sensors Beta", Sensors,
                       7, "+2 to hit (excluding missiles)")
  val ECM2     = Value("ECM2", "ECM Beta", Sensors,
                       9, "Add-on: +2 evade missiles")                                
  val Scanner2 = Value("Scanner2", "Improved space scanner", Sensors,
                       12, "Colonies can detect ships 5 ly away")
  val Sensors3 = Value("Sensors3", "Combat sensors Gamma", Sensors,
                       13, "+3 to hit (excluding missiles)")
  val ECM3     = Value("ECM3", "ECM Gamma", Sensors,
                       15, "Add-on: +3 evade missiles")
  val Scanner3 = Value("Scanner3", "Advanced space scanner", Sensors,
                       17, "Colonies can detect ships 7 ly away")
  val ECM4     = Value("ECM4", "ECM Delta", Sensors,
                       19, "Add-on: +4 evade missiles")
  val Sensors4 = Value("Sensors4", "Combat sensors Delta", Sensors,
                       20, "+4 to hit (excluding missiles)")
  val ECM5     = Value("ECM5", "ECM Epsilon", Sensors,
                       22, "+5 evade missiles")
  val Sensors5 = Value("Sensors5", "Combat sensors Epsilon", Sensors,
                       25, "+5 to hit (excluding missiles)")
  val SubComm  = Value("SubComm", "Subspace communications", Sensors,
                       26, "Ships can be redirected in warp")
  val Sensors6 = Value("Sensors6", "Combat sensors Zeta", Sensors,
                       28, "+6 to hit (excluding missiles)")
  val ECM6     = Value("ECM6", "ECM Zeta", Sensors,
                       28, "+6 evade missiles")
  val Scanner4 = Value("Scanner4", "Subspace scanner", Sensors,
                       30, "Colonies can detect ships 11 ly away")
  
  //---------------------------------------------------------------------
  // Civil - new planet types, max population, infrastructure cost, maintainence
  val Industry9   = Value("Industry9", "Industrial Tech 9", Civil,
                          2, "Reduces the cost of a factory to 9 RU")
  val MaxPop10    = Value("MaxPop10", "Coordinated zoning", Civil,
                          4, "Max population +10%")
  val Maintain80  = Value("Maintain80", "Basic conservation", Civil,
                          5, "Reduces factory maintaince to 80%")
  val IceCol      = Value("IceCol", "Arctic exploration", Civil,
                          6, "Can colonize Ice planets")
  val Industry8   = Value("Industry8", "Industrial Tech 8", Civil,
                          8, "Reduces the cost of a factory to 8 RU")
  val BarrenCol   = Value("BarrenCol", "Barren colonization", Civil,
                          9, "Can colonize Barren planets")
  val MaxPop20    = Value("MaxPop20", "Competent planning", Civil,
                          11, "Max population +20%")
  val DeadCol     = Value("DeadCol", "Ecodomes", Civil,
                          13, "Can colonize Dead planets")
  val AsteroidCol = Value("AsteroidCol", "Microgravity survival", Civil,
                          14, "Can colonize Asteroids")
  val Industry7   = Value("Industry7", "Industrial Tech 7", Civil,
                          14, "Reduces the cost of a factory to 7 RU")
  val Maintain60  = Value("Maintain60", "Systematic conservation", Civil,
                          16, "Reduces factory maintainence to 60%")
  val InfernoCol  = Value("InfernoCol", "Tectonic stabilization", Civil,
                          17, "Can colonize Inferno planets")
  val HostilePop  = Value("HostilePop", "Hostile environmental control", Civil,
                          18, "Ice/Barren/Dead/Inferno planets: +50% max pop.")
  val Industry6   = Value("Industry6", "Industrial Tech 6", Civil,
                          18, "Reduces the cost of a factory to 6 RU")
  val GasGiantCol = Value("GasGiantColonies", "Atmospheric colonization", Civil,
                          20, "Can colonize Gas Giants")
  val Industry5   = Value("Industry5", "Industrial Tech 5", Civil,
                          23, "Reduces the cost of a factory to 5 RU")
  val MaxPop30    = Value("MaxPop30", "Dense zoning", Civil,
                          24, "Max population +30%")
  val Maintain40  = Value("Maintain40", "Integrated maintainence", Civil,
                          25, "Reduces factory maintainence to 40%")
  val Industry4   = Value("Industry4", "Industrial Tech 4", Civil,
                          28, "Reduces the cost of a factory to 4 RU")
 
  // Policy - productivity and allegiance
  val Grow1   = Value("Grow1", "Colonial medicine", Policy,
                      3, "Population growth rate +20%")
  val Alleg10 = Value("Alleg10", "Humane goverance", Policy,
                      7, "Allegiance +10%")
  val Prod2   = Value("Prod2", "Competent industrial management", Policy,
                      8, "200% factory productivity")
  val Sci1    = Value("Sci1", "Specialist Economy", Policy,
                      10, "+20% research for all planets with >40M colonists")
  val Grow2   = Value("Grow2", "Incentivized colonization", Policy,
                      12, "Population growth rate +40%")
  val Alleg20 = Value("Alleg20", "Federated governance", Policy,
                      13, "+20% Allegiance colonies, allies, & occupied worlds")
  val Prod3   = Value("Prod3", "Operations research", Policy,
                      15, "300% factory productivity")
  val Grow3   = Value("Grow3", "Abundant opportunity", Policy,
                      18, "Population growth rate +70%")
  val Prod4   = Value("Prod4", "Lean manufacturing", Policy,
                      21, "400% factory productivity")
  val Sci2    = Value("Sci2", "Deep Specialization", Policy,
                      22, "+40% research for all planets with >40M colonists")
  val Alleg30 = Value("Alleg30", "Enlightened governance", Policy,
                      24, "Allegiance from all worlds: +30%")
  val Grow4   = Value("Grow4", "Colonial self-sufficiency", Policy,
                      25, "Population growth rate +100%")
  val Prod5   = Value("Prod5", "Complete automation", Policy,
                      28, "500% factory productivity")
  
  val startingTechs = List(Engines1, Gun1, Armor1)
  
  val categorizedVals = TechCategory.organizeTechs(values)
  
  // returns list of techs you can research, listed first by category, then
  // by tech level. i.e. [[propulsion1, propulsion2, ...], [weapons1...], ...]
  def generateCanResearchTechs() =
    categorizedVals.map(l => 
      Random.shuffle(l).filter(t => !startingTechs.contains(t))
        .take((0.6*l.length).toInt).sortBy(_.level))
                      
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

case class PlanetType(name: String) extends hasName

object PlanetType extends Enumerator[PlanetType] {
  val Terran    = Value("Terran")
  val Ocean     = Value("Ocean")
  val Arid      = Value("Arid")
  val Ice       = Value("Ice")
  val Barren    = Value("Barren")
  val Dead      = Value("Dead")
  val Inferno   = Value("Inferno")
  val Asteroid  = Value("Asteroid")
  val GasGiant  = Value("Gas Giant")
  
  val eclass = classOf[PlanetType]
  private def Value(name: String) =
    addToMap(PlanetType(name))
  
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
