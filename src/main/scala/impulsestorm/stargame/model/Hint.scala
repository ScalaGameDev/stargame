package impulsestorm.stargame.model

case class Hint(mapInfoChangeOnly: Boolean, 
                selectedUuid: String = "") {
  def selected(f: Fleet) = copy(selectedUuid="fv-" + f.uuid)
}

