package simulador

import simulador.ArenaDeCell.Movimiento
import simulador.ArenaDeCell.Luchadores
import simulador.TuplasUtils._

trait Item
case class UsarItem(item: Item) extends Movimiento((luchadores: Luchadores) => {
  val (l1,l2)= luchadores
  if (!luchadores._1.poseeItem(item)) {
    luchadores
  } else {
    (item, luchadores._1.raza, luchadores._2.raza) match {
      case (Roma, _, Androide)                    => luchadores
      case (Roma, _, _) if luchadores._2.ki < 300 => luchadores.onSec(_ quedateInconsciente)
      case (Filosa, _, Saiyajin(true))      => luchadores.onSec(_ cortarCola)
      case (Filosa, _, _)                         => luchadores.onSec(_.disminuirKi(luchadores._1.ki / 100))
      case (SemillaDelErmitaño, _, _)             => (l1.recuperarMaxPotencial(),l2)
      case (Fuego(cant), _, _) if cant <= 0       => luchadores
      case (Fuego(cant), _, Humano)               => (l1.disminuirMunicion(cant),l2 disminuirKi (20))
      case (Fuego(cant), _, Namekusein) if l2.estado == Inconsciente =>  (l1.disminuirMunicion(cant), l2 disminuirKi (10))
      case (Fuego(cant), _, _)                    => (l1.disminuirMunicion(cant), l2)

    }
  }
})

case object Roma extends Item
case object Filosa extends Item
case class Fuego(cant: Int = 0) extends Item
case object SemillaDelErmitaño extends Item
case class EsferasDelDragon(cant: Int) extends Item
case object FotoDeLaLuna extends Item 
