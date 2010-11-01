package impulsestorm.liftapp.model.stargame

// if not moving, will just use the fromStarId attr and the others are undef
// ships: DesignId->Number
case class Fleet( playerId: Int,
                  ships: Map[Int, Int], moving: Boolean, 
                  fromStarId: Int, toStarId: Int,
                  departClock: Double, arriveClock: Double )

object Fleet {
  def startingFleet(playerId: Int, homeStarId: Int) = {
    Fleet(playerId,
          Map(3->0, 1->1), false,
          homeStarId, 0,
          0, 0)
  }
}

case class Design( id: Int,
                   name: String,
                   active: Boolean,
                   size: ShipSize,
                   engine: Engine, 
                   sensor: Sensor, 
                   modules: List[ShipModule] )

object Design {
  val startingDesigns = 
    List(
      Design(0, "Scout", true,
             ShipSize.Fighter,
             Engine.Chemical, 
             Sensor.Alpha,
             List(ShipModule.ReserveTanks)),
      Design(1, "Colony Ship", true,
             ShipSize.Capital,
             Engine.Chemical,
             Sensor.Alpha,
             List(ShipModule.ColonyKit))
      )
}


case class ShipSize(name: String, space: Double) extends hasName

object ShipSize extends Enumerator[ShipSize] {
  val Fighter       = Value("Fighter", 40)
  val Corvette      = Value("Corvette", 200)
  val Frigate       = Value("Frigate", 1000)
  val Capital       = Value("Capital", 5000)
  
  val eclass = classOf[ShipSize]
  private def Value(name: String, space: Double) =
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
