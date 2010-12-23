package impulsestorm.stargame.model

// if not moving, will just use the fromStarId attr and the others are undef
// ships: [# of Design1, # of Design 2 etc.] 
case class Fleet( uuid: String, playerId: Int,
                  ships: List[Int], 
                  moving: Boolean, 
                  fromStarId: Int, toStarId: Option[Int],
                  departYear: Option[Double], arriveYear: Option[Double]) 
extends hasPosition {
  def bulk(s: StarGameState) =
    (s.players(playerId).designs zip ships).map{ 
      case (d, n) => d.size.space*n }.sum
      
  def speed(s: StarGameState) : Double = {
    (ships zip s.players(playerId).designs).flatMap {
      case(n, d) => if(n>0) List(d.engine.speed) else Nil
    }.min
  }
    
  def position(s: StarGameState) = {
    def coords(star: Star) = (star.x, star.y)
    
    if(moving) {
      // linear interpolation
      val alpha = 1-(s.gameYear-departYear.get)/(arriveYear.get-departYear.get)
      val (xFrom, yFrom) = coords(s.stars(fromStarId))
      val (xTo, yTo)     = coords(s.stars(toStarId.get))
      
      (alpha*xFrom+(1-alpha)*xTo, alpha*yFrom+(1-alpha)*yTo)
    } else coords(s.stars(fromStarId))
  }
  
  def newUUID = copy(uuid=java.util.UUID.randomUUID().toString())
}

object Fleet {
  val emptyQuantity = List(0,0,0,0,0,0)
  
  def startingFleet(playerId: Int, homeStar: Star) = {
    val shipCounts = List(3, 1, 0, 0, 0, 0) 
    Fleet("", playerId,
          shipCounts, false,
          homeStar.id, None,
          None, None).newUUID
  }
}

case class Design( id: Int,
                   name: String,
                   size: ShipSize,
                   engine: Engine, 
                   sensor: Option[Sensor], 
                   modules: List[ShipModule] )

object Design {
  val startingDesigns = 
    List(
      Design(0, "Scout",
             ShipSize.Fighter,
             Engine.Chemical, 
             None,
             List(ShipModule.ReserveTanks)),
      Design(1, "Colony Ship",
             ShipSize.Capital,
             Engine.Chemical,
             None,
             List(ShipModule.ColonyKit))
      )
}


case class ShipSize(name: String, space: Int) extends hasName

object ShipSize extends Enumerator[ShipSize] {
  val Fighter       = Value("Fighter", 40)
  val Corvette      = Value("Corvette", 200)
  val Frigate       = Value("Frigate", 1000)
  val Capital       = Value("Capital", 5000)
  
  val eclass = classOf[ShipSize]
  private def Value(name: String, space: Int) =
    addToMap(ShipSize(name, space))
}

case class Engine(name: String,
                  speed: Double) extends hasName

object Engine extends Enumerator[Engine] {
  val Chemical   = Value("Chemical", 1.0)
  val Ion        = Value("Ion", 1.5)
  val Nuclear    = Value("Nuclear", 2.0)
  val Fusion     = Value("Fusion", 2.5)
  val Antimatter = Value("Antimatter", 3.0)
  val Subspace   = Value("Subspace", 4.0)
  
  val eclass = classOf[Engine]
  private def Value(name: String, speed: Double) =
    addToMap(Engine(name, speed))
}

case class Sensor(name: String,
                  range: Int) extends hasName
                   
object Sensor extends Enumerator[Sensor] {
  val Alpha   = Value("Alpha", 1)
  val Beta    = Value("Beta",  2)
  val Gamma   = Value("Gamma", 3)
  val Delta   = Value("Delta", 4)
  val Epsilon = Value("Epsilon", 5)
  val Zeta    = Value("Zeta", 6)
  
  val eclass = classOf[Sensor]
  private def Value(name: String, range: Int) =
    addToMap(Sensor(name, range))
}

case class ShipModule(name: String, longName: String,
                      description: String) extends hasName

object ShipModule extends Enumerator[ShipModule] {
  val ReserveTanks = Value("ReserveTanks", "Reserve Fuel Tanks",
                           "Doubles ship range")
  val ColonyKit    = Value("ColonyKit", "Colonizing equipment",
                           "Allows the ship to establish a colony.")
  
  val eclass = classOf[ShipModule]
  private def Value(name: String, longName: String, description: String) =
    addToMap(ShipModule(name, longName, description))
}
