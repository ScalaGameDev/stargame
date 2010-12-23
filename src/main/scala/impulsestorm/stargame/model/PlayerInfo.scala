package impulsestorm.stargame.model.stargame

case class PlayerInfo(player: Player, shipRange: Int)

object PlayerInfo {
  def from(p: Player) = {
    PlayerInfo(p, p.shipRange)
  }
}
