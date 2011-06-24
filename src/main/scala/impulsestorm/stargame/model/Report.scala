package impulsestorm.stargame.model

case class BattleReport(victorId: Int, shipsRemaining: Int, 
                        loserIds: Set[Int], starId: Int, gameYear: Double)
