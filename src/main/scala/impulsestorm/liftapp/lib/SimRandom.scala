package impulsestorm.liftapp.lib

import scala.util.Random

object SimRandom {
  // FIXME - consult Simulation book on most efficient way
  def weightedRandom[ObjT](li: List[(Double, ObjT)]) = {
    val (rawWeights, objs) = li.unzip
    val sumWeights = rawWeights.sum
    
    // normalized so sum is 1, change to cumulative also
    val normalizedCumWeights =
      (1 to li.size).map(rawWeights.take(_).sum / sumWeights)
      
    // get first object that has a normalied cumulative weight greater than rand
    val rand = Random.nextDouble()
    (normalizedCumWeights zip objs).filter(_._1 > rand).head._2 
  }
  
  def randomObj[ObjT](li: List[ObjT]) =
    li(Random.nextInt(li.length))
  
  // random integer between a and b 
  def random(a: Int, b: Int) =
    a + Random.nextInt(b-a+1)
}
