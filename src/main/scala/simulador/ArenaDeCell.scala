package simulador

import scala.collection.immutable.List
import scala.collection.immutable.Set
import scala.collection.GenTraversableOnce

object ArenaDeCell {

  case class Guerrero(raza: Raza, ki: Int = 0, kiMax: Int, items: List[Item] = List(), movimientos: Set[Movimiento] = Set(), estado: Estado) {

    def aumentarKi(cuanto: Int) = copy(ki = ki + cuanto)
    def disminuirKi(cuanto: Int) = copy(ki = ki - cuanto)
    def aprenderMovimiento(unMovimiento: Movimiento) = copy(movimientos = movimientos + (unMovimiento))
    def agregarItem(unItem: Item) = copy(items = items.+:(unItem))
    def poseeItem(unItem: Item) = items.contains(unItem)
    def removerItem(unItem: Item) = copy(items = items.filterNot { item => item == unItem }) //este metodo lo hace, el que lo llama tiene que ser consciente de que tenga el item

    def recuperarMaxPotencial() = copy(ki = kiMax)

    def esbueno() = List(Saiyajin(_,_),Namekusein(),Humano()).contains(this.raza)//Ez game
    
    def disminuirMunicion(cant: Int) = {
      this removerItem (Fuego(cant))
      this agregarItem (Fuego(cant - 1))
    }

    def quedateInconsciente() = copy(estado = Inconsciente)

    def cortarCola() = this //para que tipe
  }

  trait Estado {}

  case object Inconsciente extends Estado
  case object Muerto extends Estado
  case object Normal extends Estado

  trait Item {}
  //esta parte quedo demasiado funcional pn, habría que delegar un poco y ver que se puede ahcer para no repetir tanta logica
  case class UsarItem(item: Item) extends Movimiento {
    def apply(luchadores: Luchadores) = {
      if (!luchadores._1.poseeItem(item)) {
        luchadores
      } else {
        (item, luchadores._1.raza, luchadores._2.get.raza) match {
          case (Roma(), _, _) if luchadores._2.get.ki < 300 => (luchadores._1, luchadores._2.map(defe => defe quedateInconsciente))
          case (Filosa(), _, Saiyajin(true, _)) => (luchadores._1, luchadores._2.map(defe => defe cortarCola))
          case (Filosa(), _, _) => (luchadores._1, luchadores._2.map(defe => defe disminuirKi (luchadores._1.ki / 100)))
          case (Fuego(cant), _, Humano()) if cant > 0 => (luchadores._1.disminuirMunicion(cant), luchadores._2.map(defe => defe disminuirKi (20)))
          case (Fuego(cant), _, Namekusein()) if cant > 0 && luchadores._2.get.estado == Inconsciente => (luchadores._1.disminuirMunicion(cant), luchadores._2.map(defe => defe disminuirKi (10)))
          case (Filosa(), _, Saiyajin(true, _)) => (luchadores._1, luchadores._2.map(defe => defe cortarCola))
          case (SemillaDelErmitaño(), _, _) => (luchadores._1.recuperarMaxPotencial(), luchadores._2)
        }
      }
    }
  }

  case class Roma() extends Item {}
  case class Filosa() extends Item {}
  case class Fuego(cant: Int) extends Item {}
  case class SemillaDelErmitaño() extends Item {}

  case class Magia(criterio: Luchadores => Luchadores) extends Movimiento {
    def apply(luchadores: Luchadores) = criterio(luchadores)
  }

  case class Fusion(Compañero: Guerrero) extends Movimiento {
    def apply(luchadores: Luchadores) = {
      (luchadores._1) match {
        case (luchador) if luchador.esbueno()=> luchadores//TODO insertar fusion
        case _ => luchadores
      }
    }
  }

  abstract class Raza

  case class Monstruo() extends Raza {}
  case class Humano() extends Raza {}
  case class Androide(bateria: Int = 0) extends Raza {
    def quedateInconsciente() = this
  }
  case class Namekusein() extends Raza {}
  case class Saiyajin(cola: Boolean, nivel: Int = 0) extends Raza {
    def cortarCola() = this //Va a ser implementado mas adelante
  }

  type Movimiento = Luchadores => Luchadores

  type Luchadores = (Guerrero, Option[Guerrero])

  val dejarseFajar = (luchadores: Luchadores) => luchadores //TODO posiblemente cambie cuando quiera saber los turnos

  val cargarKi = (luchadores: Luchadores) => {
    luchadores._1.raza match {
      case Androide(_)                     => luchadores
      case Saiyajin(_, nivel) if nivel > 0 => (luchadores._1.aumentarKi(150 * nivel), luchadores._2) //corregir y hacer que matchee con ss
      case _                               => (luchadores._1.aumentarKi(100), luchadores._2)
    }
  }

}