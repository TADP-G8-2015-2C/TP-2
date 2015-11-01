package simulador

import scala.collection.immutable.List
import scala.collection.immutable.Set
import scala.collection.GenTraversableOnce


object ArenaDeCell {
  
  case class Guerrero(raza: Raza, ki: Int = 0, items: List[Item] = List()
      , movimientos: Set[Movimiento] = Set(), estado: Estado) {
    
    def aumentarKi(cuanto: Int) = copy(ki = ki + cuanto)
    def disminuirKi(cuanto: Int) = copy(ki = ki - cuanto)
    
    def aprenderMovimiento(unMovimiento: Movimiento) = copy(movimientos = movimientos +(unMovimiento))
    
    def agregarItem(unItem: Item) = copy(items = items.+:(unItem))
    def poseeItem(unItem: Item) = items.contains(unItem)
    def removerItem(unItem: Item) = copy(items = items.filterNot { item => item == unItem })//este metodo lo hace, el que lo llama tiene que ser consciente de que tenga el item
    
    def quedateInconsciente() = copy (estado = Inconsciente)
  }
  
  
  trait Estado {}
  
  case object Inconsciente extends Estado
  case object Muerto extends Estado
  case object Normal extends Estado
  
  abstract class Item {
    def aplicateSobre(luchadores: Luchadores)
  }
  /*
  case class Roma() extends Item{
    def aplicateSobre(luchadores: Luchadores) = {
      val defensor = luchadores._2
      val atacante = luchadores._1
      
      if(defensor.get.ki < 300 && defensor.get.raza != Androide()){
          (atacante, defensor.get.quedateInconsciente())
      }
      else {
        luchadores
      }
      
    }
  }
  
  
  case class Filosa() extends Item{
    def aplicateSobre(luchadores: Luchadores) = {luchadores}
  }
  case class Fuego() extends Item{
    def aplicateSobre(luchadores: Luchadores) = {luchadores}
  }
  
  */
  abstract class Raza
  
  case class Monstruo() extends Raza {}
  case class Humano() extends Raza {}
  case class Androide(bateria: Int = 0) extends Raza {}
  case class Namekusein() extends Raza {}
  case class Saiyajin(cola: Boolean, nivel: Int = 0) extends Raza {}

  type Movimiento = Luchadores => Luchadores
  
  type Luchadores = (Guerrero, Option[Guerrero])
  
  
  val dejarseFajar = (luchadores: Luchadores) => luchadores//TODO posiblemente cambie cuando quiera saber los turnos
  
  val cargarKi = (luchadores: Luchadores) => {
    luchadores._1.raza match {
      case Androide(_) => luchadores
      case Saiyajin(_, nivel) => (luchadores._1.aumentarKi(150 * nivel), luchadores._2)//corregir y hacer que matchee con ss
      case _ => (luchadores._1.aumentarKi(100), luchadores._2)
    }
  }
  /*
  case class usarItem(itemAUsar: Item) extends Movimiento {
    def apply(luchadores: Luchadores) = {
      val usador = luchadores._1
      val atacante = luchadores._2
      
      if(!usador.poseeItem(itemAUsar)){
        luchadores
      }
      else {
        itemAUsar.aplicateSobre(luchadores)
      }
    }
    
  }*/
  
}