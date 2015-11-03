package simulador

import scala.collection.immutable.List
import scala.collection.immutable.Set
import scala.collection.GenTraversableOnce

object ArenaDeCell {

  case class Magia(criterio: Luchadores => Luchadores) extends Movimiento {
    def apply(luchadores: Luchadores) = {
      (luchadores._1, luchadores._1.raza) match {
        case (_, Namekusein()) => criterio(luchadores)
        case (_, Monstruo())   => criterio(luchadores)
        case (guerrero, _) if guerrero.poseeItem(SieteEsferasDelDragon()) =>
          criterio(guerrero.removerItem(SieteEsferasDelDragon()), luchadores._2)
        case (_, _) => luchadores
      }
    }
  }

  case class Fusion(Compañero: Guerrero) extends Movimiento {
    def apply(luchadores: Luchadores) = {
      (luchadores._1, luchadores._2.get) match {
        case (luchador, compañero) if luchador.esbueno() && compañero.esbueno() => (luchador.fusionateCon(compañero), luchadores._2)
        case _ => luchadores
      }
    }
  }

  case class ComerseAOtro(criterio: Luchadores => Set[Movimiento]) extends Movimiento {
    def apply(luchadores: Luchadores) = {
      (luchadores._1.raza, luchadores._2.get.ki) match {
        case (Monstruo(), kiOponente) if luchadores._1.ki > kiOponente => (luchadores._1.copy(movimientos = criterio(luchadores)), luchadores._2.map(l2 => l2 morite))
        case (_, _) => luchadores
      }
    }
  }

  type Movimiento = Luchadores => Luchadores

  type Luchadores = (Guerrero, Option[Guerrero])

  val dejarseFajar = (luchadores: Luchadores) => luchadores //TODO posiblemente cambie cuando quiera saber los turnos

  val cargarKi = (luchadores: Luchadores) => {
    luchadores._1.raza match {
      case Androide(_)                        => luchadores
      case Saiyajin(_, nivel, _) if nivel > 0 => (luchadores._1.aumentarKi(150 * nivel), luchadores._2) //corregir y hacer que matchee con ss
      case _                                  => (luchadores._1.aumentarKi(100), luchadores._2)
    }
  }

  val convertirseEnMonoGigante = (luchadores: Luchadores) => {
    (luchadores._1, luchadores._1.raza) match {
      case (guerrero, Saiyajin(true, nivel, false)) if guerrero.poseeItem(FotoDeLaLuna()) => (guerrero.dejarDeSerSS.transformateEnMono, luchadores._2)
      case (_, _) => luchadores
    }
  }
  val ConvertirseEnSS = (luchadores: Luchadores) => {
    (luchadores._1.raza) match {
      case (Saiyajin(_, nivel, false)) => (luchadores._1.copy(raza = luchadores._1.raza.subirDeNivel(luchadores._1), kiMax = (nivel + 1) * 5 * luchadores._1.kiMax), luchadores._2)
      case (_)                         => luchadores
    }
  }

  /*
     *  Cuando un Saiyajin se vuelve muy poderoso se convierte en Super Saiyajin, estas transformaciones son acumulables 
     *  (eso quiere decir que cuando un SS se vuelve muy fuerte se puede convertir en SS nivel 2, luego en SS nivel 3 y así...). 
     *  Para poder convertirse en SS o pasar al siguiente nivel, el ki del Saiyajin debe estar, por lo menos, por la mitad de su máximo actual. 
     *  Al transformarse, el máximo ki del guerrero se multiplica por 5 por cada nivel de Super Saiyajin, pero su ki no aumenta. 
     *  Si el guerrero queda inconsciente o se transforma en mono el estado de SS se pierde.
     */
}