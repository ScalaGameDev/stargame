package impulsestorm.stargame.model

case class PlayerInfo(player: Player, shipRange: Int)

object PlayerInfo {
  def from(p: Player) = {
    PlayerInfo(p, p.shipRange)
  }
}
