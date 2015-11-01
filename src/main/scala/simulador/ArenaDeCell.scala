package simulador

import scala.collection.immutable.List
import scala.collection.GenTraversableOnce


object ArenaDeCell {
  
  case class Guerrero(raza: Raza, ki: Int = 0, items: List[Item] = List(), movimientos: List[Movimiento] = List()) {

  def aumentarKi(cuanto: Int) = copy(ki = ki + cuanto)
  def disminuirKi(cuanto: Int) = copy(ki = ki - cuanto)
  def learnMovement(unMovimiento: Movimiento) = copy(movimientos = movimientos.+:(unMovimiento))
  def agregarItem(unItem: Item) = copy(items = items.+:(unItem))
  def removerItem(unItem: Item) = copy(items = items.filterNot { item => item == unItem })//este metodo lo hace, el que lo llama tiene que ser consciente de que tenga el item
}
  
  trait Item {}
  
  trait Raza {}
  case class Monster() extends Raza {}
  case class Human() extends Raza {}
  case class Android() extends Raza {}
  case class Namekusein() extends Raza {}
  case class Saiyajin() extends Raza {}
  
  type Movimiento = Function1[Guerrero, Option[Guerrero]]
  case class DejarseFarjar() extends Movimiento{}
  
}