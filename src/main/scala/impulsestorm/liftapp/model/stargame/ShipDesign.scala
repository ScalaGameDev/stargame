package impulsestorm.liftapp.model.stargame

// if not moving, will just use the fromStarId attr and the others are undef
// ships: [# of Design1, # of Design 2 etc.] 
case class Fleet( playerId: Int,
                  ships: List[Int], bulk: Int, moving: Boolean, 
                  fromStarId: Int, toStarId: Option[Int],
                  departClock: Option[Double], arriveClock: Option[Double],
                  x: Double, y: Double) 
  extends hasPosition

object Fleet {
  def startingFleet(playerId: Int, homeStar: Star) = {
    val shipCounts = List(3, 1) 
    Fleet(playerId,
          shipCounts, calculateBulk(Design.startingDesigns, shipCounts), false,
          homeStar.id, None,
          None, None, homeStar.x, homeStar.y)
  }
  
  def calculateBulk(designs: List[Design], shipCounts: List[Int]) =
    (designs zip shipCounts).map{ case (d, n) => d.size.space*n }.sum
}

case class Design( id: Int,
                   name: String,
                   active: Boolean,
                   size: ShipSize,
                   engine: Engine, 
                   sensor: Option[Sensor], 
                   modules: List[ShipModule] )

object Design {
  val startingDesigns = 
    List(
      Design(0, "Scout", true,
             ShipSize.Fighter,
             Engine.Chemical, 
             None,
             List(ShipModule.ReserveTanks)),
      Design(1, "Colony Ship", true,
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
                  speed: Int) extends hasName

object Engine extends Enumerator[Engine] {
  val Chemical   = Value("Chemical", 1)
  val Ion        = Value("Ion", 2)
  val Nuclear    = Value("Nuclear", 3)
  val Fusion     = Value("Fusion", 4)
  val Antimatter = Value("Antimatter", 5)
  val Subspace   = Value("Subspace", 6)
  
  val eclass = classOf[Engine]
  private def Value(name: String, speed: Int) =
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
