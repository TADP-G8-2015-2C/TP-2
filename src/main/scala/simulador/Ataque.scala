package simulador

import simulador.ArenaDeCell.Movimiento
import simulador.TuplasUtils._
import simulador.ArenaDeCell.Luchadores

/*
  abstract class Ataque(ataque: (Luchadores => Luchadores)) extends Movimiento((luchadores: Luchadores) => {
    def apply(luchadores: Luchadores): Luchadores = {
      (luchadores._1.estado, luchadores._2.estado, this) match {
        case _ => ataque(luchadores)
      }
    }
  })*/
/*
abstract class AtaqueEnergia(ataque: Function[Luchadores, (Int,Int)]) 
                                              extends Movimiento((luchadores: Luchadores) => {
  override def apply(luchadores: Luchadores): Luchadores = {
   super apply(ataque)
     val (kiGastado, kiDañado) = ataque(luchadores)
     (luchadores._1.raza,luchadores._2.raza) match {
       case (Androide(bateria1),Androide(bateria2))  => (luchadores._1.copy(raza = Androide(bateria1 -kiGastado)),
                                        luchadores._2.copy(raza = Androide(bateria2 + kiDañado)))
        case (Androide(bateria1),_)  => (luchadores._1.copy(raza = Androide(bateria1 -kiGastado)), 
                                        luchadores._2.disminuirKi(kiDañado))
        case (_,Androide(bateria2))  => (luchadores._1.disminuirKi(kiGastado), 
                                        luchadores._2.copy(raza = Androide(bateria2 + kiDañado)))
       case (_,_) => luchadores.//(luchadores._1.disminuirKi(kiGastado), luchadores._2.disminuirKi(kiDañado))
     }
  }
}) */

//case class Fisico(ataque: Movimiento) extends Ataque {}

case object muchosGolpesNinja extends Movimiento((luchadores: Luchadores) => {
  (luchadores._1.raza, luchadores._2.raza) match {
    case (Humano, Androide) => (luchadores._1.disminuirKi(10), luchadores._2)
    case (_, _) => if ((luchadores._1.ki > luchadores._2.ki)) {
      (luchadores._1, luchadores._2 disminuirKi (20))
    } else {
      (luchadores._1.disminuirKi(20), luchadores._2)
    }
  }
})

case object explotar extends Movimiento((luchadores: Luchadores) => {
  (luchadores._1.raza, luchadores._2.raza) match {
    case (Monstruo, Namekusein) => (luchadores._1.morite(), luchadores._2 disminuirKiNamekusein (luchadores._1.ki * 2))
    case (Androide, Namekusein) => (luchadores._1.morite(), luchadores._2 disminuirKiNamekusein (luchadores._1.ki * 3))
    case (Monstruo, _)          => (luchadores._1.morite(), luchadores._2 disminuirKi (luchadores._1.ki * 2))
    case (Androide, _)          => (luchadores._1.morite(), luchadores._2 disminuirKi (luchadores._1.ki * 3))
    case (_, _)                 => luchadores
  }
})

abstract class AtaqueEnergico(unAtaque: Luchadores =>(Int,Int)) extends Movimiento((luchadores: Luchadores) => {
   val (ki1,ki2) = unAtaque(luchadores)
  (luchadores._1.raza, luchadores._2.raza) match {
      case (_, Androide)  => (luchadores._1.disminuirKi(ki1), luchadores._2.aumentarKi(ki2))
      case (_, _)         => (luchadores._1.disminuirKi(ki1), luchadores._2.disminuirKi(ki2))
    }
})

case class onda(kiNecesario: Int) extends AtaqueEnergico((luchadores: Luchadores) => {
  val (l1,l2) = luchadores
  if (l1.ki >= kiNecesario)
      (l1.raza, l2.raza) match {
      case (_, Monstruo) => (kiNecesario, kiNecesario / 2)
      case (_, Androide) => (kiNecesario,kiNecesario)
      case (_, _) => (kiNecesario, kiNecesario * 2)
    }
  else
    (0,0)
})

case object genkidama extends AtaqueEnergico((luchadores: Luchadores) => {
  val (l1,l2) = luchadores
  (l1.estado, l2.raza) match {
    case (NiUnaMenos(rounds), _) => (0, math.pow(10, rounds).asInstanceOf[Int])
    case _                       => (0,0)
  }
})


