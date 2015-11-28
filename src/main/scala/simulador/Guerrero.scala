package simulador

import scala.util.Try
import scala.collection.immutable.List
import scala.collection.immutable.Set
import scala.collection.GenTraversableOnce
import simulador.ArenaDeCell.Movimiento
import simulador.ArenaDeCell.Luchadores
import simulador.ArenaDeCell._
import scala.util.Try

case class Guerrero(raza: Raza, ki: Int = 0, kiMax: Int, items: List[Item] = List(), movimientos: Set[Movimiento] = Set(), 
    estado: Estado, fase: Fase = FaseInicial) {

  def aumentarKi(cuanto: Int) = {
    if (cuanto + ki > kiMax) {
      copy(ki = kiMax)
    } else {
      copy(ki = ki + cuanto)
    }
  }

  def disminuirKi(cuanto: Int) = {
    if (ki - cuanto <= 0) {
      copy(ki = 0, estado = Muerto)
    } else {
      copy(ki = ki - cuanto)
    }
  }
  def disminuirKiNamekusein(cuanto: Int) = {
    if (ki - cuanto < 1) {
      disminuirKi(ki - 1)
    } else {
      disminuirKi(cuanto)
    }
  }

  def aprenderMovimiento(unMovimiento: Movimiento) = copy(movimientos = movimientos + (unMovimiento))
  def sabeMovimiento(unMovimiento: Movimiento) = movimientos.contains(unMovimiento)
  def agregarItem(unItem: Item) = copy(items = items.+:(unItem))
  def poseeItem(unItem: Item) = items.contains(unItem)
  def removerItem(unItem: Item) = copy(items = items.filterNot { item => item == unItem }) //este metodo lo hace, el que lo llama tiene que ser consciente de que tenga el item

  def poseer7Esferas = {
    List.range(1, 8).forall { n => this.poseeItem(EsferasDelDragon(n)) }
  }

  def usar7Esferas: Guerrero = {
    if (this.poseer7Esferas) List.range(1, 8).foldLeft(this)((l, n) => { l.removerItem(EsferasDelDragon(n)) })
    else this
  }

  def recuperarMaxPotencial() = copy(ki = kiMax)

  def esbueno() = List(Saiyajin(_), Namekusein, Humano).contains(this.raza) //Ez game
  def esMalo() = List(Androide, Monstruo).contains(this.raza) //no se puede hacer el contrario por la raza fusion ¬¬

  def disminuirMunicion(cant: Int) = {
    removerItem(Fuego(cant))
      .agregarItem(Fuego(cant - 1))
  }

  def dejarDeSerSS() = {
    (fase) match {
      case (SSJ(nivel)) if nivel > 1 => copy(fase = FaseInicial, kiMax = kiMax / (5 * (nivel - 1))).aumentarKi(0)
      case (_)                       => this
    }
  }

  def fusionateCon(compañero: Guerrero) = {
    copy(raza = Fusionado(this), ki = ki + compañero.ki, kiMax = kiMax + compañero.kiMax, movimientos = movimientos ++ compañero.movimientos)

  }

  def subirDeNivel() = {
    fase match {
      case SSJ(nivel) if ki >= kiMax => copy(fase = SSJ(nivel + 1))
      case _                         => this
    }
  }

  def morite() = copy(estado = Muerto, ki = 0)
  def teFajaron() = {
    estado match {
      case NiUnaMenos(cantRounds) => copy(estado = NiUnaMenos(cantRounds + 1))
      case _  => copy(estado = NiUnaMenos())
    }
  }
  def quedateInconsciente() = dejarDeSerSS().copy(estado = Inconsciente)

  def quedateNormal() = copy(estado = Normal)

  def cortarCola() = {
    (fase, raza) match {
      case (MonoGigante, Saiyajin(true)) => copy(raza = Saiyajin(false), ki = 1, kiMax = raza.disminuirKiMax(this), 
          estado = Inconsciente, fase = FaseInicial)
      case  (_, Saiyajin(true)) => copy(Saiyajin(false),  ki = 1)
      case _ => this
    }
  }

  def transformateEnMono = copy(kiMax = kiMax * 3, ki = kiMax, fase = MonoGigante)

  def movimientoMasEfectivoContra(enemigo: Guerrero)(criterio: CriterioDeCombate): Option[Movimiento] = {
    if (movimientos isEmpty) return None
    val luchadores = (this, enemigo)
    val movMasEfectivo = movimientos.maxBy { mov => criterio(mov(luchadores)) }

    if (luchadores._1.estado == Muerto || criterio(movMasEfectivo(luchadores)) <= 0) None
    else Some(movMasEfectivo)
  }

  def pelearUnRound(movElegido: Movimiento, criterio: CriterioDeCombate = mayorVentajaKi)(enemigo: Guerrero): Luchadores = {
    val luchadores = movElegido(this, enemigo).swap
    val movContraAtaque: Option[Movimiento] = luchadores._1.movimientoMasEfectivoContra(luchadores._2)(criterio)
    movContraAtaque.fold(luchadores)(_(luchadores)).swap
  }
  
  type PlanDeAtaque = List[Movimiento]

  def planDeAtaqueContra(enemigo: Guerrero, cantidadDeRounds: Int)(unCriterio: CriterioDeCombate): Try[List[Movimiento]] = Try {
    val luchadores = (this, enemigo)
    val plan: List[Movimiento] = List()
    val tupla = (luchadores, plan)

    List.range(1, cantidadDeRounds + 1).foldLeft(luchadores, plan)({
      case (((atacante, defensor), plan), _) =>
        atacante.movimientoMasEfectivoContra(defensor)(unCriterio)
          .fold(throw new NoHayMovMasEfectivoParaGenerarPlanException("Fallo el plan"))(m =>
            (atacante.pelearUnRound(m, unCriterio)(defensor), plan.:+(m)))
    })._2

  }

  def pelearContra(enemigo: Guerrero)(unPlan: PlanDeAtaque): ResultadoDePelea = {

    def pelearDadoMovimiento(resultado: ResultadoDePelea, movimiento: Movimiento): ResultadoDePelea = {
      for {
        (atacante, enemigo) <- resultado
      } yield (atacante.pelearUnRound(movimiento)(enemigo))
      /*
      resultado match {
        case SiguenPeleando(luchadores) => ResultadoDePelea(luchadores._1.pelearUnRound(movimiento)(luchadores._2)())
        case Ganador(luchador)          => Ganador(luchador) //Falta arreglar aca
      }*/
    }
    val semilla: ResultadoDePelea = SiguenPeleando(this, enemigo)
    unPlan.foldLeft(ResultadoDePelea(this, enemigo))(pelearDadoMovimiento)
  }

}
object ResultadoDePelea {
  def apply(luchadores: Luchadores): ResultadoDePelea = {
    val (atacante, enemigo) = luchadores
    (atacante.estado, enemigo.estado) match {
      case (Muerto, _) => Ganador(enemigo)
      case (_, Muerto) => Ganador(atacante)
      case _           => SiguenPeleando(atacante, enemigo)
    }
  }
  def apply(guerrero: Guerrero): ResultadoDePelea = Ganador(guerrero)
}

trait ResultadoDePelea {
  def map(m: Luchadores => Luchadores): ResultadoDePelea

  def filter(criterio: Luchadores => Boolean): ResultadoDePelea

  def fold[T](z: T)(op: Luchadores => T): T

  def flatMap(f: Luchadores => ResultadoDePelea): ResultadoDePelea
}
case class SiguenPeleando(luchadores: Luchadores) extends ResultadoDePelea {

  def map(m: Luchadores => Luchadores): ResultadoDePelea = ResultadoDePelea(m(luchadores))

  def filter(criterio: Luchadores => Boolean): ResultadoDePelea = ResultadoDePelea(luchadores) // = //Ni idea -- this? o ResultadoDePelea(luchadores)

  def fold[T](z: T)(op: Luchadores => T): T = op(luchadores)

  def flatMap(f: Luchadores => ResultadoDePelea): ResultadoDePelea = f(luchadores) //  = ResultadoDePelea(f(luchadores))//esto sería unapply
}
case class Ganador(luchador: Guerrero) extends ResultadoDePelea { //el ganador se comporta como el [] de las listas 

  def map(m: Luchadores => Luchadores): ResultadoDePelea = this

  def filter(criterio: Luchadores => Boolean): ResultadoDePelea = this

  def fold[T](z: T)(op: Luchadores => T): T = z

  def flatMap(f: Luchadores => ResultadoDePelea): ResultadoDePelea = this
}

trait Estado

case object Inconsciente extends Estado
case object Muerto extends Estado
case object Normal extends Estado
case class NiUnaMenos(cantRounds: Int = 1) extends Estado

trait Fase

case object FaseInicial extends Fase
case object MonoGigante extends Fase
case class SSJ(nivel: Int = 1) extends Fase
  
