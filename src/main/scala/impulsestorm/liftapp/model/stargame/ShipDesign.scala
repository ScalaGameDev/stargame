package impulsestorm.liftapp.model.stargame

// if not moving, will just use the fromStarId attr and the others are undef
case class Fleet( ships: Map[ShipDesign, Int], moving: Boolean, 
                  fromStarId: Int, toStarId: Int,
                  departClock: Double, arriveClock: Double )

case class ShipDesign( engines: Engine.Value )

object Engine extends MyEnum {
  val Chemical   = Value("Chemical")
  val Ion        = Value("Ion")
  val Nuclear    = Value("Nuclear")
  val Fusion     = Value("Fusion")
  val Antimatter = Value("Antimatter")
  val Subspace   = Value("Subspace")
  
  val warpSpeed : Map[Value, Double] = Map(Chemical   -> 1,
                                           Ion        -> 2,
                                           Nuclear    -> 3,
                                           Fusion     -> 4,
                                           Antimatter -> 5,
                                           Subspace   -> 6)
}
