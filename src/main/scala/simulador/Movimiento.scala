package simulador

import scala.collection.immutable.List
import scala.collection.immutable.Set
import scala.collection.GenTraversableOnce
import simulador._

import simulador.TuplasUtils._

object ArenaDeCell {
  type Luchadores = (Guerrero, Guerrero)

  abstract class Movimiento(movimiento: (Luchadores => Luchadores)) extends Function1[Luchadores, Luchadores] {
    def apply(luchadores: Luchadores): Luchadores = {
      (luchadores._1.estado, luchadores._2.estado, this) match {
        case (Muerto, _, _) => luchadores
        case (Inconsciente, _, UsarItem(SemillaDelErmitaño)) => movimiento(luchadores.onFst(_ quedateNormal))
        case (Inconsciente, _, _) => luchadores
        case (_, _, movimientoPosta) if movimientoPosta != DejarseFajar => (movimiento(luchadores)._1.copy(roundsFajado = 0), movimiento(luchadores)._2)
        case _ => movimiento(luchadores)
      }
    }
  }

  case class Magia(cambiarEstado: Function1[Luchadores, Luchadores]) extends Movimiento((luchadores: Luchadores) => {
    (luchadores._1, luchadores._1.raza) match {
      case (_, Namekusein) => cambiarEstado(luchadores)
      case (_, Monstruo) => cambiarEstado(luchadores)
      case (guerrero, _) if guerrero.poseeItem(EsferasDelDragon(7)) => cambiarEstado(luchadores.onFst(_ usar7Esferas))
      case (_, _) => luchadores
    }
  })

  case class Fusion(compañero: Guerrero) extends Movimiento((luchadores: Luchadores) => {
    (luchadores._1) match {
      case (luchador) if luchador.esbueno() && compañero.esbueno() => (luchador.fusionateCon(compañero), luchadores._2)
      case _ => luchadores
    }
  })

  case class ComerseAOtro(digestion: Function1[Luchadores, Option[Set[Movimiento]]]) extends Movimiento((luchadores: Luchadores) => {
   /*  for {
       (l1,l2) <-luchadores
       if(l1.raza == Monstruo && l1.ki > l2.ki)
       mov1 <- digestion(l1,l2)
     }
      mov1.fold(luchadores){mov=>(luchadores._1.copy(movimientos = mov), luchadores._2 morite)}*/
    (luchadores._1.raza, luchadores._2.ki) match {
      case (Monstruo, kiOponente) if luchadores._1.ki > kiOponente => digestion(luchadores).fold(luchadores){mov=>(luchadores._1.copy(movimientos = mov), luchadores._2 morite)} 
      case (_, _) => luchadores
    }
  })

  case object DejarseFajar extends Movimiento((luchadores: Luchadores) => {
    luchadores.onFst(_.teFajaron())
  })

  case object cargarKi extends Movimiento((luchadores: Luchadores) => {
    val (l1, l2) = luchadores
    (l1.raza, l1.fase) match {
      case (Androide, _)                          => luchadores
      case (Saiyajin(_), SSJ(nivel)) if nivel > 0 => luchadores.onFst(_.aumentarKi(150 * nivel)) //como saiyan
      case _                                      => luchadores.onFst(_.aumentarKi(100))
    }
  })

  case object convertirseEnMonoGigante extends Movimiento((luchadores: Luchadores) => {
    val (l1, l2) = luchadores
    (l1.raza, l1.fase) match {
      case (Saiyajin(true), fase) if l1.poseeItem(FotoDeLaLuna) & fase != MonoGigante =>
        (l1.dejarDeSerSS.transformateEnMono.copy(raza = Saiyajin(true)), l2)
      case _ => luchadores
    }
  })

  case object ConvertirseEnSS extends Movimiento((luchadores: Luchadores) => {
    val (l1, l2) = luchadores
    (l1.raza, l1.fase) match {
      case (Saiyajin(cola), SSJ(nivel)) if l1.ki * 2 > l1.kiMax => //como no mono
        (l1.subirDeNivel().copy(kiMax = (nivel + 1) * 5 * l1.kiMax), l2)
      case (Saiyajin(cola), FaseInicial) if l1.ki * 2 > l1.kiMax => //como no mono
        (l1.copy(fase=SSJ(), kiMax = 5 * l1.kiMax),l2)
      case _ => luchadores
    }
  })

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