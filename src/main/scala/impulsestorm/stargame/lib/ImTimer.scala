package impulsestorm.stargame.lib

import java.util.TimerTask

object ImTimer extends java.util.Timer {
  def addTask(f: ()=>Unit, delay: Long, period: Long) = {
    
    val task = new TimerTask {
      def run() = f()
    }
    
    scheduleAtFixedRate(task, delay, period)
    
    task
  }
}
