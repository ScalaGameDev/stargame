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
  val Computers   = Value("Computers")  // 0
  val Ecology     = Value("Ecology")    // 1
  val Industry    = Value("Industry")   // 2
  val Weaponry    = Value("Weaponry")   // 3
  val Shielding   = Value("Shielding")  // 4
  val Propulsion  = Value("Propulsion") // 5
  
  val eclass = classOf[TechCategory]
  private def Value(name: String) =
    addToMap(TechCategory(name))
  
  val defaultAllocation : List[Double] = List.fill(6)(1.0/6)
}

case class Tech(name: String) extends hasName

object Tech extends Enumerator[Tech] {
  val eclass = classOf[Tech]
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
