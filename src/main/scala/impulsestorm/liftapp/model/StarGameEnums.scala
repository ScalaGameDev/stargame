package impulsestorm.liftapp.model

import impulsestorm.liftapp.lib.SimRandom._

import net.liftweb.json._
import net.liftweb.json.JsonAST._

class EnumSerializer[E <: Enumeration: ClassManifest](val enum: E)
  extends Serializer[E#Value] {

  import JsonDSL._
  val EnumerationClass = classOf[enum.Value] 

  println(enum.toString)
  println(EnumerationClass)
  
  def deserialize(implicit format: Formats):
    PartialFunction[(TypeInfo, JValue), E#Value] = {
    case (TypeInfo(x, z), json) if x==EnumerationClass => json match {
      case JString(value) => {
        println(x)
        enum.withName(value)
      }
      case value => 
        throw new MappingException("Can't convert " +
                                   value + " to "+ EnumerationClass)
    }
  }
  
  def serialize(implicit format: Formats): PartialFunction[Any,JValue] = {
    case i: E#Value => i.toString
  }
}

object EnumSerializers {
  val li = 
    List(TechCategory, Tech, Trait, StarClass, PlanetZone, PlanetType).map(
      new EnumSerializer(_))
  
  val formats: Formats = li.foldLeft(Serialization.formats(NoTypeHints))(_+_)
}

object TechCategory extends Enumeration {
  val Computers   = Value("Computers")
  val Ecology     = Value("Ecology")
  val Industry    = Value("Industry")
  val Weaponry    = Value("Weaponry")
  val Shielding   = Value("Shielding")
  val Propulsion  = Value("Propulsion")
}

object Tech extends Enumeration {
}

object Trait extends Enumeration {
  val Militaristic  = Value("Militaristic")
  val Scientific    = Value("Scientific")
  val Expansionist  = Value("Expansionist")
  val Organized     = Value("Organized")
  val Industrious   = Value("Industrious")
  val Diplomatic    = Value("Diplomatic")
  val Commercial    = Value("Commercial")  
}

object StarClass extends Enumeration {
  val Giant   = Value("Giant")
  val B       = Value("B")
  val A       = Value("A")
  val F       = Value("F")
  val G       = Value("G")
  val K       = Value("K")
  val M       = Value("M")
  val Compact = Value("Compact")
  
  val abundances = 
    List( 0.03->Giant,
          0.03->B,
          0.05->A,
          0.10->F,
          0.12->G,
          0.12->K,
          0.40->M,
          0.19->Compact )
  
  def randomNPlanets(sClass: Value) = {
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
  
  def randomZone(sClass: Value, totalPlanets: Int) =
    if      ( List(Giant, B, A, M) contains sClass )
      randomObj(PlanetZone.listZones)
    else if ( List(F, G, K)        contains sClass )
      // shift probability towards outer for 6 or more planets
      weightedRandom( 
        List( math.min(1/3.0, 0.5-totalPlanets/32.0)->PlanetZone.Inner,
              math.min(1/3.0, 0.5-totalPlanets/32.0)->PlanetZone.Middle,
              math.max(1/3.0, totalPlanets/16.0)    ->PlanetZone.Outer ) )
    else PlanetZone.Outer
    
    
}

object PlanetZone extends Enumeration {
  val Inner  = Value("Inner")
  val Middle = Value("Middle")
  val Outer  = Value("Outer")
  
  val listZones = this.values.toList
  
  def randomPType(sClass: StarClass.Value, 
                  zone: PlanetZone.Value) : PlanetType.Value = {
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

object PlanetType extends Enumeration {
  val Terran    = Value("Terran")
  val Ocean     = Value("Ocean")
  val Arid      = Value("Arid")
  val Ice       = Value("Ice")
  val Barren    = Value("Barren")
  val Dead      = Value("Dead")
  val Inferno   = Value("Inferno")
  val Asteroid  = Value("Asteroid")
  val GasGiant  = Value("Gas Giant")
  
  val listTypes = this.values.toList
}
