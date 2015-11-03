package simulador

import simulador.ArenaDeCell.Movimiento
import simulador.ArenaDeCell.Luchadores

trait Item {}
//esta parte quedo demasiado funcional pn, habría que delegar un poco y ver que se puede ahcer para no repetir tanta logica
case class UsarItem(item: Item) extends Movimiento {
  def apply(luchadores: Luchadores) = {
    if (!luchadores._1.poseeItem(item)) {
      luchadores
    } else {
      (item, luchadores._1.raza, luchadores._2.raza) match {
        case (Roma(), _, Androide(_))                     => luchadores
        case (Roma(), _, _) if luchadores._2.ki < 300 => (luchadores._1, luchadores._2 quedateInconsciente)
        case (Filosa(), _, Saiyajin(true, _, _))          => (luchadores._1, luchadores._2 cortarCola)
        case (Filosa(), _, _)                             => (luchadores._1, luchadores._2 disminuirKi (luchadores._1.ki / 100))
        case (Fuego(cant), _, _) if cant <= 0             => luchadores
        case (Fuego(cant), _, Humano()) => (luchadores._1.disminuirMunicion(cant),
          luchadores._2 disminuirKi (20))
        case (Fuego(cant), _, Namekusein()) if luchadores._2.estado == Inconsciente =>
          (luchadores._1.disminuirMunicion(cant), luchadores._2 disminuirKi (10))
        case (Fuego(cant), _, _)          => (luchadores._1.disminuirMunicion(cant), luchadores._2)
        case (SemillaDelErmitaño(), _, _) => (luchadores._1.recuperarMaxPotencial(), luchadores._2)
      }
    }
  }
}

case class Roma() extends Item {}
case class Filosa() extends Item {}
case class Fuego(cant: Int = 0) extends Item {}
case class SemillaDelErmitaño() extends Item {}
case class SieteEsferasDelDragon() extends Item {}
case class FotoDeLaLuna() extends Item {}
