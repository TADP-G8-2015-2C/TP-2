package warrior

import scala.collection.immutable.List
import item.Item
import movement.Movement
import scala.collection.GenTraversableOnce

case class Warrior(race: Race, ki: Int = 0, items: List[Item] = List(), movements: List[Movement] = List()) {

  def increaseKi(quantity: Int) = copy(ki = ki + quantity)
  def decreaseKi(quantity: Int) = copy(ki = ki - quantity)
  def learnMovement(aMovement: Movement) = copy(movements = movements.+:(aMovement))
  def addItem(aItem: Item) = copy(items = items.+:(aItem))
  def removeItem(aItem: Item) = copy(items = items.filterNot { item => item == aItem })//este metodo lo hace, el que lo llama tiene que ser consciente de que tenga el item

  def mostEffectiveMovementAgainst(opponent: Warrior) = "Should be implemented"
  def roundFight(aMovement: Movement, opponent: Warrior) = "Should be implemented"
  def attackPlanAgainst(opponent: Warrior, rounds: Int) = "Should be implemented"

}