package impulsestorm.stargame.model

case class PlayerInfo(player: Player, 
                      range: Int, 
                      speed: Double,
                      sensorRange: Int,
                      maxPopMultiplier: Double)

object PlayerInfo {
  def from(p: Player) = {
    PlayerInfo(p, p.range, p.speed, p.sensorRange, p.maxPopMultiplier)
  }
}
