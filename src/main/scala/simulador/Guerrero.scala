package simulador

import scala.collection.immutable.List
import scala.collection.immutable.Set
import scala.collection.GenTraversableOnce
import simulador.ArenaDeCell.Movimiento
import simulador.ArenaDeCell.Luchadores
import simulador.ArenaDeCell._

case class Guerrero(raza: Raza, ki: Int = 0, kiMax: Int, items: List[Item] = List(), movimientos: Set[Movimiento] = Set(), estado: Estado) {

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

  def esbueno() = List(Saiyajin(_, _, _), Namekusein, Humano).contains(this.raza) //Ez game
  def esMalo() = List(Androide, Monstruo).contains(this.raza) //no se puede hacer el contrario por la raza fusion ¬¬

  def disminuirMunicion(cant: Int) = {
    removerItem(Fuego(cant))
      .agregarItem(Fuego(cant - 1))
  }

  def dejarDeSerSS() = {
    (raza) match {
      case (Saiyajin(cola, nivel, false)) if nivel > 1 => copy(raza = Saiyajin(cola, 1, false), kiMax = kiMax / (5 * (nivel - 1))).aumentarKi(0)
      case (_)                                         => this
    }
  }

  def fusionateCon(compañero: Guerrero) = {
    copy(raza = Fusionado(this), ki = ki + compañero.ki, kiMax = kiMax + compañero.kiMax, movimientos = movimientos ++ compañero.movimientos)

  }

  def morite() = copy(estado = Muerto, ki = 0)
  def quedateNiUnaMenos() = {
    estado match {
      case NiUnaMenos(turnos) => copy(estado = NiUnaMenos(turnos + 1))
      case _                  => copy(estado = NiUnaMenos())
    }
  }
  def quedateInconsciente() = raza.meQuedeInconsciente(this).copy(estado = Inconsciente)

  def quedateNormal() = copy(estado = Normal)

  def cortarCola() = copy(raza = raza.cortarCola, ki = raza.kiLuegoDeCortarCola(this), kiMax = raza.disminuirKiMax(this))

  def transformateEnMono = copy(kiMax = kiMax * 3, ki = kiMax)

  

  def movimientoMasEfectivoContra(enemigo: Guerrero)(criterio: CriterioDeCombate): Movimiento = {
    if (movimientos isEmpty) throw new NoTieneMovimientosException("Antes que aprenda algun movimiento")
    val luchadores = (this, enemigo)
    val movMasEfectivo = movimientos.maxBy { mov => criterio(mov(luchadores)) }
    if (criterio(movMasEfectivo(luchadores)) <= 0)
      throw new NoTieneMovimientoMasEfectivoException("No tiene movimiento mas efectivo contra oponente")
    else movMasEfectivo
  }


  //Si bien así es mejor porque podemos definirle el criterio de contrataque al enemigo,
  //cuando se llama a este método pelearUnRound, nos obliga a agregarle el tercer parámetro,
  //haciendo que nuestro método no se ejecute con la misma interfaz del que está en enunciado.
  def pelearUnRound(movElegido: Movimiento)(enemigo: Guerrero)(criterio: CriterioDeCombate = mayorVentajaKi): Luchadores = {
    val luchadores = movElegido(this, enemigo)
    val movContraAtaque = luchadores._2.movimientoMasEfectivoContra(luchadores._1)(criterio)
    val luchadoresFinal = movContraAtaque(luchadores._2, luchadores._1)
    (luchadoresFinal._2, luchadoresFinal._1)
  }

  def planDeAtaqueContra(enemigo: Guerrero, cantidadDeRounds: Int)(unCriterio: CriterioDeCombate): List[Movimiento] = {
    val luchadores = (this, enemigo)
    val plan: List[Movimiento] = List()
    val tupla = (luchadores, plan)

    List.range(1, cantidadDeRounds + 1).foldLeft(luchadores, plan)((tupla, pepita) => {
      val ((atacante, defensor), plan) = tupla
      val atacante.pelearUnRound(movIntermedio)(defensor)(unCriterio)movIntermedio: Movimiento = atacante.movimientoMasEfectivoContra(defensor)(unCriterio)
      (movIntermedio(atacante, defensor), plan.:+(movIntermedio))
    })._2

  }

  type PlanDeAtaque = List[Movimiento]

  def planDeAtaqueContra2(enemigo: Guerrero, cantidadDeRounds: Int)(unCriterio: CriterioDeCombate): PlanDeAtaque = {

    val luchadores = (this, enemigo)
    val planDeAtaque: PlanDeAtaque = List()

    val agregarMovmientoAlPlanLuegoDeEjecutar = (luchadoresYPlan: (Luchadores, PlanDeAtaque), round: Int) => {
      val luchadoresAhora = luchadoresYPlan._1
      val plan = luchadoresYPlan._2

      val movimientoAAgregar = luchadoresAhora._1.movimientoMasEfectivoContra(luchadoresAhora._2)(unCriterio)

      (luchadoresAhora._1.pelearUnRound(movimientoAAgregar)(luchadoresAhora._2)(), plan.+:(movimientoAAgregar))
    }

    List.range(1, cantidadDeRounds + 1).foldLeft(luchadores, planDeAtaque)(agregarMovmientoAlPlanLuegoDeEjecutar)._2
  }

  //ahora le meto Unit porque puede devolver o un Guerrero o Luchadores. Más adelante
  //ver de hacer algo mejor...
  //boceto muy básico, no cumple con lo pedido...
  //investigar cómo salir de foldLeft o ver de usar find o algo así...
  def pelearContra(enemigo: Guerrero)(unPlan: PlanDeAtaque): Unit = {

    val luchadores = (this, enemigo)

    val pelearDadoMovimiento = (luchadores: Luchadores, movimiento: Movimiento) => {
      val atacante = luchadores._1
      val enemigo = luchadores._2

      atacante.pelearUnRound(movimiento)(enemigo)()
    }

    unPlan.foldLeft(luchadores)(pelearDadoMovimiento)
  }
}


  
trait Estado

case object Inconsciente extends Estado
case object Muerto extends Estado
case object Normal extends Estado
case class NiUnaMenos(turnos: Int = 1) extends Estado
  
