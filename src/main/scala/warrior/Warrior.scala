package warrior

import scala.collection.immutable.List
import item.Item
import movement.Movement
import scala.collection.mutable.Set


class Warrior(var ki: Int = 0, var items:Set[Item] = Set(),var movements:Set[Movement] = Set()) { 
  //quizas case class y el set quizás podría ser inmutable
  
  def mostEffectiveMovementAgainst(opponent: Warrior) = "Should be implemented"
  def roundFight(aMovement: Movement,opponent: Warrior) = "Should be implemented"
  def attackPlanAgainst(opponent: Warrior, rounds: Int) = "Should be implemented"
  
  
    
}