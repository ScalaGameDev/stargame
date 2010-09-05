package impulsestorm.liftapp.model

import _root_.net.liftweb.mapper._ 
/*
class StarGame extends LongKeyedMapper[StarGame] with IDPK {
  def getSingleton = StarGame
  
  object name extends MappedString(this, 64) {
    override def defaultValue = "Untitled Stargame"
  }
  
  object startTime extends MappedDateTime(this) {
    override def defaultValue = new java.util.Date
  }
  
  // this field is always set to a positive natural number
  object clockRate extends MappedDouble(this) {
    override def defaultValue = 1.0
  }
  
  object map extends 
  
  def clock = {
    val secondsOld = ( (new java.util.Date).getTime - start.getTime ) / 1000
    secondsOld * clockRate.get
  }
}

object StarGame extends StarGame with LongKeyedMetaMapper[StarGame] {
}*/
