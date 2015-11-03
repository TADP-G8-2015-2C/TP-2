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

  trait Ataque {}
  case class Energia(ataque: Movimiento) extends Ataque {}
  case class Fisico(ataque: Movimiento) extends Ataque {}

  val muchosGolpesNinja = (luchadores: Luchadores) => {
    (luchadores._1.raza, luchadores._2.get.raza) match {
      case (Humano(), Androide(_)) => (luchadores._1.disminuirKi(10), luchadores._2)
      case (_, _) => if ((luchadores._1.ki > luchadores._2.get.ki)) {
        (luchadores._1, luchadores._2.map(a => a.disminuirKi(20)))
      } else {
        (luchadores._1.disminuirKi(20), luchadores._2)
      }
    }
  }

  val Explotar = (luchadores: Luchadores) => {
    (luchadores._1.raza, luchadores._2.get.raza) match {
      case (Monstruo(), Namekusein())        => (luchadores._1.morite(), luchadores._2.map(l2 => l2 disminuirKiNamekusein (luchadores._1.ki * 2)))
      case (Androide(bateria), Namekusein()) => (luchadores._1.morite(), luchadores._2.map(l2 => l2 disminuirKi (bateria * 3)))
      case (Monstruo(), _)                   => (luchadores._1.morite(), luchadores._2.map { l2 => l2 disminuirKi (luchadores._1.ki * 2) })
      case (Androide(bateria), _)            => (luchadores._1.morite(), luchadores._2.map(l2 => l2 disminuirKi (bateria * 3)))
      case (_, _)                            => luchadores
    }
  }
  case class onda(kiNecesario: Int) extends Movimiento {
    def apply(luchadores: Luchadores) = {//quizas con un if y una abstraccion me podría asegurar de que siempre tenga energia necesaria
      (luchadores._1.raza, luchadores._2.get.raza) match {
        case (Androide(bateria), Monstruo()) if bateria >= kiNecesario => (luchadores._1.disminuirKi(kiNecesario), luchadores._2.map(l2 => l2 disminuirKi (kiNecesario /2)))
        case (Androide(bateria), _) if bateria >= kiNecesario => (luchadores._1.disminuirKi(kiNecesario), luchadores._2.map(l2 => l2 disminuirKi (kiNecesario *2)))
        case (_, Monstruo()) if luchadores._1.ki >= kiNecesario => (luchadores._1.disminuirKi(kiNecesario), luchadores._2.map(l2 => l2 disminuirKiNamekusein (kiNecesario / 2)))
        case (_, _) if luchadores._1.ki >= kiNecesario => (luchadores._1.disminuirKi(kiNecesario), luchadores._2.map { l2 => l2 disminuirKi (kiNecesario *2) })
        case (_, _) => luchadores
      }
    }//ES MUY FEO ESTO MUCHISIMA LOGICA REPETIDA MATAN CONEJITOS
  }

  /*   case class atacarCon(ataque: Ataque) extends Movimiento {
      def apply(luchadores: Luchadores) = {
        (ataque,luchadores._1,luchadores._2.get) match{
          case (Energia)
          
        }
      }
    }*/

  /*
     *  Cuando un Saiyajin se vuelve muy poderoso se convierte en Super Saiyajin, estas transformaciones son acumulables 
     *  (eso quiere decir que cuando un SS se vuelve muy fuerte se puede convertir en SS nivel 2, luego en SS nivel 3 y así...). 
     *  Para poder convertirse en SS o pasar al siguiente nivel, el ki del Saiyajin debe estar, por lo menos, por la mitad de su máximo actual. 
     *  Al transformarse, el máximo ki del guerrero se multiplica por 5 por cada nivel de Super Saiyajin, pero su ki no aumenta. 
     *  Si el guerrero queda inconsciente o se transforma en mono el estado de SS se pierde.
     */
}