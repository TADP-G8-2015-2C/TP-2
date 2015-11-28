package simulador

import simulador.ArenaDeCell.Movimiento
import simulador.TuplasUtils._
import simulador.ArenaDeCell.Luchadores

case object muchosGolpesNinja extends Movimiento((luchadores: Luchadores) => {
  (luchadores._1.raza, luchadores._2.raza) match {
    case (Humano, Androide) => (luchadores._1.disminuirKi(10), luchadores._2)
    case (_, _) => 
      if ((luchadores._1.ki > luchadores._2.ki)) 
      (luchadores._1, luchadores._2 disminuirKi (20))
     else 
      (luchadores._1.disminuirKi(20), luchadores._2)
    
  }
})

case object explotar extends Movimiento((luchadores: Luchadores) => {
    val(l1,l2) = luchadores
    val explotables: List[Raza] = List(Monstruo, Androide)
    if (explotables.contains(l1.raza))
    (l1.morite(), (luchadores._1.raza, luchadores._2.raza) match {
    case (Monstruo, Namekusein) => l2 disminuirKiNamekusein (l1.ki * 2)
    case (Androide, Namekusein) => l2 disminuirKiNamekusein (l1.ki * 3)
    case (Monstruo, _)          => l2 disminuirKi (l1.ki * 2)
    case (Androide, _)          => l2 disminuirKi (l1.ki * 3)
  })
    else
      luchadores
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
  l1.estado match {
    case NiUnaMenos(cantRounds) => (0, math.pow(10, cantRounds).asInstanceOf[Int]) 
    case _ =>(0, 1) 
  }
  
})


