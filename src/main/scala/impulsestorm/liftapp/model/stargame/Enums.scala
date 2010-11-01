package impulsestorm.liftapp.model.stargame

import impulsestorm.liftapp.lib.SimRandom._

import net.liftweb.json._
import net.liftweb.json.JsonAST._

trait hasName {
  val name: String
}

trait Enumerator[EnumeratedT <: hasName] {
  val eclass : Class[EnumeratedT]
  
  def withName(name: String) = vmap(name)
  
  def addToMap(instance: EnumeratedT) = {
    vmap = vmap + (instance.name->instance)
    values = instance :: values
    instance
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

  
  val eclass = classOf[TechCategory]
  private def Value(name: String) =
    addToMap(TechCategory(name))
  
  val defaultAllocation : List[Double] = List.fill(6)(1.0/6)
}

case class Tech(name: String, category: TechCategory, level: Int, 
                description: String) 
  extends hasName

object Tech extends Enumerator[Tech] {
  import TechCategory.{Propulsion, Weapons, Protection, Sensors, Civil, Policy}
  
  val ChemicalEngines    = Value("Chemical engines", Propulsion, 1,
                                 "Equipped ships move at warp speed 1")
  val HydrogenFuelCells  = Value("Hydrogen fuel cells", Propulsion, 2,
                                 "Ships have a range of 4")
  val DeuteriumFuelCells = Value("Deuterium fuel cells", Propulsion, 5,
                                 "Ships have a range of 5")
  val IonEngines         = Value("Ion engines", Propulsion, 6,
                                 "Ships move at warp speed 2")
  val TritiumFuelCells   = Value("Tritium fuel cells", Propulsion, 9,
                                 "Ships have a range of 6")
  val VectoredThrust     = Value("Vectored thrust", Propulsion, 10,
                                 "Add-on module for fighters and corvettes: +2 manuver")
  val NuclearEngines     = Value("Nuclear engines", Propulsion, 12,
                                 "Equipped ships move at warp speed 3")
  val IrridiumFuelCells  = Value("Irridium fuel cells", Propulsion, 14,
                                 "Ships have a range of 7")
  val BurstThrust        = Value("Burst power", Propulsion, 16,
                                 "All fighters and corvettes: +2 manuver")
  val FusionEngines      = Value("Fusion engines", Propulsion, 18,
                                 "Equipped ships mave at warp speed 4")
  val UridiumFuelCells   = Value("Uridium fuel cells", Propulsion, 19,
                                 "Ships have a range of 8")
  val InertialDampener   = Value("Inertial dampener", Propulsion, 20,
                                 "Add-on module for frigates and capitals: +2 manuver")
  val ReajaxIIFuelCells  = Value("Reajax II fuel cells", Propulsion, 23,
                                 "Ships have a range of 9")
  val AntimatterEngines  = Value("Antimatter engines", Propulsion, 24,
                                 "Equipped ships move at warp speed 5")
  val CapitalManuvering  = Value("Captial manuvering", Propulsion, 27,
                                 "All frigates and capitals: +2 manuver")
  val AntimatterFuel     = Value("Antimatter fuel cells", Propulsion, 29,
                                 "Ships have a range of 10")
  val SubspaceEngines    = Value("Subspace engines", Propulsion, 30,
                                 "Eqipped ships move at warp speed 6")
                                
  //---------------------------------------------------------------------
  val Laser              = Value("Laser", Weapons, 1,
                                 "1-4 energy damage")
  val Missiles           = Value("Missiles", Weapons, 1,
                                 "4 damage to target, +1 to hit")
  val Chaingun           = Value("Chaingun", Weapons, 1,
                                 "2-8 physical damage, -1 to hit")                               
  val PlanetaryBombs     = Value("Planetary bombs", Weapons, 2,
                                 "3-12 damage to planetary targets only")
  val ThermiteMissiles   = Value("Thermite Missiles", Weapons, 4,
                                 "6 damage to target, +1 to hit")
  val GatlingLasers      = Value("Gatling lasers", Weapons, 5,
                                 "1-4 energy damage, 4x fire rate")
  val FlakCannon         = Value("Flak cannon", Weapons, 7,
                                 "1-4 physical damage, +3 to hit fighters and corvettes")
  val BusterRockets      = Value("Buster rockets", Weapons, 8,
                                 "10 damage, -1 to hit")
  val FusionBombs        = Value("Fusion bombs", Weapons, 9,
                                 "4-16 damage to planetary targets only")
  val CapacitorLaser     = Value("Capacitor laser", Weapons, 10,
                                 "3-8 energy damage")
  val ScatterPkMissiles  = Value("Scatter pack missiles", Weapons, 11,
                                 "Splits into 5 missiles doing 6 damage each")
  val Coilgun            = Value("Coilgun", Weapons, 13,
                                 "5-11 physical damage")
  val SeekerMissiles     = Value("Seeker missiles", Weapons, 14,
                                 "10 damage, +2 to hit")
  val ProtonBeam         = Value("Proton beam", Weapons, 15,
                                 "4-10 energy damage")
  val AntimatterBomb     = Value("Antimatter bombs", Weapons, 16,
                                 "5-20 damage to planetary targets only")
  val StingerMissiles    = Value("Stringer missiles", Weapons, 18,
                                 "15 damage, +3 to hit")
  val Railgun            = Value("Railgun", Weapons, 19,
                                 "15-30 physical damage, 1/2 fire rate")
  val NeutronBeam        = Value("Neutron beam", Weapons, 21,
                                 "4-16 energy damage")
  val HeavyFlakCannon    = Value("Heavy flak cannon", Weapons, 22,
                                 "1-8 physical damage, +3 to hit fighters and corvettes")
  val OmegaBomb          = Value("Omega bomb", Weapons, 23,
                                 "10-50 damage to planetary targets only")
  val AntimatterTorpedos = Value("Antimatter torpedos", Weapons, 24,
                                 "30 damage")
  val HelicalRailgun     = Value("Helical railgun", Weapons, 26,
                                 "15-30 physical damage")
  val SmartMissiles      = Value("Smart missiles", Weapons, 27,
                                 "25 damage, +5 to hit")
  val SweepBeam          = Value("Autoblaster", Weapons, 29,
                                 "4-16 energy damage, 3x fire rate")
  val HardBeam           = Value("Hard beam", Weapons, 30,
                                 "6-30 energy damage")
                                 
  //---------------------------------------------------------------------
  val TitaniumArmor      = Value("Titanium armor", Protection, 1,
                                 "A lightweight material for ship hulls")
  val ClassIShields      = Value("Class I shields", Protection, 1,
                                 "Reduces energy damage by 1")
  val ClassIIShields     = Value("Class II shields", Protection, 4,
                                 "Reduces energy damage by 2")
  val DuralloyArmor      = Value("Duralloy armor", Protection, 8,
                                 "Equipping ships have 150% base HP")
  val ClassIIIShields    = Value("Class III Shields", Protection, 10,
                                 "Reduces energy damage by 3, other types by 1")
  val ClassVPlanetary    = Value("Class V Planetary Shields", Protection, 12,
                                 "Reduces all damage taken by planetary targets by 5.")
  val AutomatedRepair    = Value("Automated Repair Module", Protection, 14,
                                 "Add-on module: Repairs 15% of remaining HP per turn.")
  val ClassIVShields     = Value("Class IV Shields", Protection, 14,
                                 "Reduces energy damage by 4, other types by 2")
  val NeutroniumArmor    = Value("Neutronium armor", Protection, 17,
                                 "Equipping ships have 200% base HP")
  val ClassVShields      = Value("Class V Shields", Protection, 20,
                                 "Reduces energy damage taken by 5, other types by 2")
  val ClassXPlanetary    = Value("Class X Planetary Shields", Protection, 22,
                                 "Reduces all damage taken by planetary targets by 10.")
  val ClassVIShields     = Value("Class VI Shields", Protection, 22,
                                 "Reduces energy damage taken by 6, other types by 3")
  val MetamaterialArmor  = Value("Metamaterial armor", Protection, 26,
                                 "Equipping ships have 250% base HP")
  val ClassVIIShields    = Value("Class VII Shields", Protection, 27,
                                 "Reduces energy damage taken by 7, other types by 4")
  val MissileShield      = Value("Missile Shield", Protection, 28,
                                 "Add-on module: gives 75% chance of destroying incoming missiles, -1% per missile tech level.")
  val AdvancedDamageCtrl = Value("Advanced damage control", Protection, 30,
                                 "Add-on module: Repairs 30% of remaining HP per turn")
  
   
                                 
  
  
  
  
  val eclass = classOf[Tech]
  
  private def Value(name: String, category: TechCategory, level: Int, 
                    description: String) =
    addToMap(Tech(name, category, level, description))
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
    case Ice      => random(10.0, 30.0)
    case Barren   => random(5.0,  20.0)
    case Dead     => random(3.0,  15.0)
    case Inferno  => random(2.0,  10.0)
    case Asteroid => random(1.0,  2.0)
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
