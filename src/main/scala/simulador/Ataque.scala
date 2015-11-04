package simulador

import simulador.ArenaDeCell.Movimiento
import simulador.ArenaDeCell.Luchadores

trait Ataque {}
case class Energia(ataque: Movimiento) extends Ataque {}
case class Fisico(ataque: Movimiento) extends Ataque {}

case class muchosGolpesNinja() extends Movimiento ((luchadores: Luchadores) => {
    (luchadores._1.raza, luchadores._2.raza) match {
      case (Humano(), Androide(_)) => (luchadores._1.disminuirKi(10), luchadores._2)
      case (_, _) => if ((luchadores._1.ki > luchadores._2.ki)) {
        (luchadores._1, luchadores._2 disminuirKi(20))
      } else {
        (luchadores._1.disminuirKi(20), luchadores._2)
      }
    }
})

case class Explotar() extends Movimiento ((luchadores: Luchadores) => {
    (luchadores._1.raza, luchadores._2.raza) match {
      case (Monstruo(), Namekusein())        => (luchadores._1.morite(), luchadores._2 disminuirKiNamekusein (luchadores._1.ki * 2))
      case (Androide(bateria), Namekusein()) => (luchadores._1.morite(), luchadores._2 disminuirKi (bateria * 3))
      case (Monstruo(), _)                   => (luchadores._1.morite(), luchadores._2 disminuirKi (luchadores._1.ki * 2) )
      case (Androide(bateria), _)            => (luchadores._1.morite(), luchadores._2 disminuirKi (bateria * 3))
      case (_, _)                            => luchadores
    }
})

case class onda(kiNecesario: Int) extends Movimiento ((luchadores: Luchadores) => {
 //quizas con un if y una abstraccion me podría asegurar de que siempre tenga energia necesaria
    (luchadores._1.raza, luchadores._2.raza) match {
      case (Androide(bateria), Monstruo()) if bateria >= kiNecesario => (luchadores._1.disminuirKi(kiNecesario), luchadores._2 disminuirKi (kiNecesario / 2))
      case (Androide(bateria), _) if bateria >= kiNecesario => (luchadores._1.disminuirKi(kiNecesario), luchadores._2 disminuirKi (kiNecesario * 2))
      case (_, Monstruo()) if luchadores._1.ki >= kiNecesario => (luchadores._1.disminuirKi(kiNecesario), luchadores._2 disminuirKiNamekusein (kiNecesario / 2))
      case (_, _) if luchadores._1.ki >= kiNecesario => (luchadores._1.disminuirKi(kiNecesario), luchadores._2 disminuirKi (kiNecesario * 2) )
      case (_, _) => luchadores
    }
 //ES MUY FEO ESTO MUCHISIMA LOGICA REPETIDA MATAN CONEJITOS
})
case object genkidama extends Movimiento((luchadores: Luchadores) => {
  (luchadores._1.estado) match {
    case (NiUnaMenos(rounds)) =>(luchadores._1, luchadores._2.disminuirKi(10^rounds))
    case _ => luchadores
  }
})

  /*   case class atacarCon(ataque: Ataque) extends Movimiento {
      def apply(luchadores: Luchadores) = {
        (ataque,luchadores._1,luchadores._2.get) match{
          case (Energia)
          
        }
      }
    }*/

