package impulsestorm.stargame.model

case class PlayerInfo(player: Player, range: Int, speed: Double)

object PlayerInfo {
  def from(p: Player) = {
    PlayerInfo(p, p.range, p.speed)
  }
}