package impulsestorm.liftapp.lib

import se.scalablesolutions.akka.stm._
import se.scalablesolutions.akka.stm.local.atomic

import net.liftweb.common.SimpleActor
import java.util.Date

// StateType is an immutable case class type
class StateSupervisor[StateType <: State, SMasterT <: StateMaster[StateType]](
  newMaster: (String) => SMasterT) extends Actor {
  
  val activeStateMasters = TransactionalMap[String, Ref[(Date, SMasterT)]]
  
  def spawnNewStateMaster(id: String) = {
    val newSM = newMaster(id)
    newSM.readFromStorage(id)
    Ref(new Date, newSM)
  }
  
  def mutate(id: String, mutateFunc: StateType => (StateType, Any)) = atomic {
    // ensure existence of StateMaster
    val smRef = activeStateMasters.getOrElseUpdate(id, spawnNewStateMaster(id))
    
    // if smRef is null, it means we just killed the StateMaster, retry...
    if(smRef.isEmpty) retry
    
    val sm = smRef.get._2
    
    sm.mutate(mutateFunc)
    
    
    
  }
  
}

// Coordinates mutation, persistence, and notification of ONE state
trait StateMaster[StateType <: State] {
  
  val id = Ref[String]
  val state = Ref[StateType]
  
  // mutateFunc out to give: newState and a hint as to what changed
  def mutate(mutateFunc: StateType => (StateType, Any)) = atomic {
    val (newstate, hint) = mutateFunc(state.get) 
    state set newstate
    notifyAll(newstate, hint)
  }
  
  def readFromStorage(storageId: String) = atomic {
    state set rawReadFromStorage(storageId)
    id set state.get.id
  }
  
  def saveToStorage() = atomic {
    id set rawSaveToStorage()
  }
  
  // observer pattern stuff
  val listeners = TransactionalVector[SimpleActor[Any]]()
  
  // sub/unsub does NOT do safety checking
  def subscribe(listener: SimpleActor[Any]) = atomic {
    listeners.add(listener)
  }
  
  def unsubscribe(listener: SimpleActor[Any]) = atomic {
    listeners.drop(listeners.findIndexOf(_ == listener))
  }
  
  def notifyAll(newstate: StateType, hint: Any) = {
    listeners.foreach( _ ! (newstate, hint)) 
  }
  
  // non-atomic reading from durable storage. must return state instance
  def rawReadFromStorage(id: String) : StateType
  
  // non-atomic saving to durable storage, must return ID
  def rawSaveToStorage() : String 
}

trait State {
  val id : String
}

