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
        case (Inconsciente, _, UsarItem(SemillaDelErmitaño)) => movimiento(luchadores.onFst (_ quedateNormal ))
        case (Inconsciente, _, _) => luchadores
        case(NiUnaMenos(_),_,movimientoPosta) if movimientoPosta != DejarseFajar  => (movimiento(luchadores)._1.copy(estado=Normal),movimiento(luchadores)._2)  
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
    (luchadores._1, luchadores._2) match {
      case (luchador, compañero) if luchador.esbueno() && compañero.esbueno() => (luchador.fusionateCon(compañero), luchadores._2)
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
    luchadores.onFst ( _.quedateNiUnaMenos() )   } )
 
  case object cargarKi extends Movimiento((luchadores: Luchadores) => {
    luchadores._1.raza match {
      case Androide                        => luchadores
      case Saiyajin(_, nivel, false) if nivel > 0 => luchadores.onFst ( _.aumentarKi(150 * nivel) ) 
      case _                                  => luchadores.onFst (_.aumentarKi(100) ) }
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
      case (Saiyajin(_, nivel, false)) => (luchadores._1.copy(raza = luchadores._1.raza.subirDeNivel(luchadores._1), kiMax = (nivel + 1) * 5 * luchadores._1.kiMax), luchadores._2)
      case (_)                         => luchadores
    }
  })

  /*
     *  Cuando un Saiyajin se vuelve muy poderoso se convierte en Super Saiyajin, estas transformaciones son acumulables 
     *  (eso quiere decir que cuando un SS se vuelve muy fuerte se puede convertir en SS nivel 2, luego en SS nivel 3 y así...). 
     *  Para poder convertirse en SS o pasar al siguiente nivel, el ki del Saiyajin debe estar, por lo menos, por la mitad de su máximo actual. 
     *  Al transformarse, el máximo ki del guerrero se multiplica por 5 por cada nivel de Super Saiyajin, pero su ki no aumenta. 
     *  Si el guerrero queda inconsciente o se transforma en mono el estado de SS se pierde.
     */
}