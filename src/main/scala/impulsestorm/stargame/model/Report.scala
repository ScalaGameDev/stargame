package impulsestorm.stargame.model

case class BattleReport(victorId: Int, victorCasualties: Int, 
                        loserIds: Set[Int], starId: Int, gameYear: Double)
