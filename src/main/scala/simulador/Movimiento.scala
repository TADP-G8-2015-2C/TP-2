package simulador

import scala.collection.immutable.List
import scala.collection.immutable.Set
import scala.collection.GenTraversableOnce

import simulador.TuplasUtils._

object ArenaDeCell {
  type Luchadores = (Guerrero, Guerrero)

  abstract class Movimiento(movimiento: (Luchadores => Luchadores)) extends Function1[Luchadores, Luchadores] {
    def apply(luchadores: Luchadores): Luchadores = {
      (luchadores._1.estado, luchadores._2.estado, this) match {
        case (Muerto, _, _) => luchadores
        case (Inconsciente, _, UsarItem(SemillaDelErmitaño)) => movimiento(luchadores.onFst(_ quedateNormal))
        case (Inconsciente, _, _) => luchadores
        case (NiUnaMenos(_), _, movimientoPosta) if movimientoPosta != DejarseFajar => (movimiento(luchadores)._1.copy(estado = Normal), movimiento(luchadores)._2)
        case _ => movimiento(luchadores)
      }
    }
  }

  case class Magia(cambiarEstado: Function1[Luchadores, Luchadores]) extends Movimiento((luchadores: Luchadores) => {
    (luchadores._1, luchadores._1.raza) match {
      case (_, Namekusein) => cambiarEstado(luchadores)
      case (_, Monstruo)   => cambiarEstado(luchadores)
      case (guerrero, _) if guerrero.poseeItem(EsferasDelDragon(7)) =>
        cambiarEstado(luchadores.onFst { l => l.usar7Esferas })
      case (_, _) => luchadores
    }
  })

  case class Fusion(compañero: Guerrero) extends Movimiento((luchadores: Luchadores) => {
    (luchadores._1) match {
      case (luchador) if luchador.esbueno() && compañero.esbueno() => (luchador.fusionateCon(compañero), luchadores._2)
      case _ => luchadores
    }
  })

  case class ComerseAOtro(digestion: Function1[Luchadores, Set[Movimiento]]) extends Movimiento((luchadores: Luchadores) => {
    (luchadores._1.raza, luchadores._2.ki) match {
      case (Monstruo, kiOponente) if luchadores._1.ki > kiOponente => (luchadores._1.copy(movimientos = digestion(luchadores)), luchadores._2 morite)
      case (_, _) => luchadores
    }
  })

  case object DejarseFajar extends Movimiento((luchadores: Luchadores) => {
    luchadores.onFst(_.quedateNiUnaMenos())
  })

  case object cargarKi extends Movimiento((luchadores: Luchadores) => {
    luchadores._1.raza match {
      case Androide                               => luchadores
      case Saiyajin(_, nivel, false) if nivel > 0 => luchadores.onFst(_.aumentarKi(150 * nivel))
      case _                                      => luchadores.onFst(_.aumentarKi(100))
    }
  })

  case object convertirseEnMonoGigante extends Movimiento((luchadores: Luchadores) => {
    (luchadores._1, luchadores._1.raza) match {
      case (guerrero, Saiyajin(true, nivel, false)) if guerrero.poseeItem(FotoDeLaLuna) =>
        (luchadores._1.dejarDeSerSS.transformateEnMono.copy(raza = Saiyajin(true, 1, true)), luchadores._2)
      case (_, _) => luchadores
    }
  })

  case object ConvertirseEnSS extends Movimiento((luchadores: Luchadores) => {
    (luchadores._1.raza) match {
      case (Saiyajin(cola, nivel, false)) if luchadores._1.ki * 2 > luchadores._1.kiMax => (luchadores._1.copy(raza = Saiyajin(cola, nivel + 1, false), kiMax = (nivel + 1) * 5 * luchadores._1.kiMax), luchadores._2)
      case (_) => luchadores
    }
  })

  val formaDigerirDeCell = (luchadores: Luchadores) => {
    (luchadores._2.raza) match {
      case Androide => luchadores._1.movimientos ++ luchadores._2.movimientos
      case _        => luchadores._1.movimientos
    }
  }

  val formaDigerirDeMajinBuu = (luchadores: Luchadores) => {
    luchadores._2.movimientos
  }

  abstract class CriterioDeCombate(criterio: (Luchadores => Int)) extends Function1[Luchadores, Int] {
    def apply(luchadores: Luchadores): Int = {
      criterio(luchadores)
    }
  }

  case object gastaMenosKi extends CriterioDeCombate((luchadores: Luchadores) => { luchadores._1.ki })
  case object mayorVentajaKi extends CriterioDeCombate((luchadores: Luchadores) => {
    luchadores._1.ki - luchadores._2.ki
  })
  case object mayorDaño extends CriterioDeCombate((luchadores: Luchadores) => {
    if (luchadores._2.ki == 0) 1 else 1 / luchadores._2.ki
  })
  case object oponenteConMasKi extends CriterioDeCombate((luchadores: Luchadores) => {
    luchadores._2.ki
  })
  case object perderMenorCantDeItems extends CriterioDeCombate((luchadores: Luchadores) => {
    luchadores._1.items.size
  })
  case object noMeMato extends CriterioDeCombate((luchadores: Luchadores) => {
    if (luchadores._1.estado == Muerto) 0 else 1
  })

}