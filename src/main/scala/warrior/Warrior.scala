package warrior

import scala.collection.immutable.List
import item.Item
import movement.Movement

case class Warrior(race: Race, ki: Int = 0, items: List[Item] = List(), movements: List[Movement] = List()) {

  def increaseKi(quantity: Int) = copy(ki = ki + quantity)
  def decreaseKi(quantity: Int) = copy(ki = ki - quantity)
//  def learnMovement(movement: Movement) = copy(movements = movements ++ movement)

  def mostEffectiveMovementAgainst(opponent: Warrior) = "Should be implemented"
  def roundFight(aMovement: Movement, opponent: Warrior) = "Should be implemented"
  def attackPlanAgainst(opponent: Warrior, rounds: Int) = "Should be implemented"

}