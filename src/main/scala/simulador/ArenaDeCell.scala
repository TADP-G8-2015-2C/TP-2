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
  
  abstract class Raza
  
  case class Monstruo() extends Raza {}
  case class Humano() extends Raza {}
  case class Androide(bateria: Int) extends Raza {}
  case class Namekusein() extends Raza {}
  case class Saiyajin(cola: Boolean, nivel: Int = 0) extends Raza {}

  type Movimiento = Luchadores => Luchadores
  
  type Luchadores = (Guerrero, Option[Guerrero])
  
  val dejarseFajar = (luchadores: Luchadores) => luchadores
  
  val cargarKi = (luchadores: Luchadores) => {
    luchadores._1.raza match {
      case Androide(_) => luchadores
      case Saiyajin(cola, nivel) => (luchadores._1.aumentarKi(150 * nivel), luchadores._2)
      case _ => (luchadores._1.aumentarKi(100), luchadores._2)
    }
  }
  
  
  
}