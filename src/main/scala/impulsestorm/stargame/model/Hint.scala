package impulsestorm.stargame.model

case class Hint(selectedUuid: String)

object Hint {
  def apply(f: Fleet) : Hint = apply("fv-" + f.uuid)
}
