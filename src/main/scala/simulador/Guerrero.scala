package simulador


import scala.collection.immutable.List
import scala.collection.immutable.Set
import scala.collection.GenTraversableOnce
import simulador.ArenaDeCell.Movimiento

case class Guerrero(raza: Raza, ki: Int = 0, kiMax: Int, items: List[Item] = List(), movimientos: Set[Movimiento] = Set(), estado: Estado) {

    def aumentarKi(cuanto: Int) = {
      if (cuanto + ki > kiMax) {
        copy(ki = kiMax)
      } else {
        copy(ki = ki + cuanto)
      }
    }

    def disminuirKi(cuanto: Int) = {
      if (ki - cuanto < 0) {
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
    def agregarItem(unItem: Item) = copy(items = items.+:(unItem))
    def poseeItem(unItem: Item) = items.contains(unItem)
    def removerItem(unItem: Item) = copy(items = items.filterNot { item => item == unItem }) //este metodo lo hace, el que lo llama tiene que ser consciente de que tenga el item

    def recuperarMaxPotencial() = copy(ki = kiMax)

    def esbueno() = List(Saiyajin(_, _, _), Namekusein(), Humano()).contains(this.raza) //Ez game
    def esMalo() = List(Androide(_), Monstruo()).contains(this.raza) //no se puede hacer el contrario por la raza fusion ¬¬

    def disminuirMunicion(cant: Int) = {
      removerItem(Fuego(cant))
        .agregarItem(Fuego(cant - 1))
    }

    def dejarDeSerSS() = {
      (raza) match {
        case (Saiyajin(cola, nivel, false)) => copy(raza = Saiyajin(cola, 0, false), kiMax = kiMax / (5 * (nivel - 1))).aumentarKi(0)
        case (_)                            => this
      }
    }

    def fusionateCon(compañero: Guerrero) = {
      copy(raza = Fusionado(this), ki = ki + compañero.ki, kiMax = kiMax + compañero.kiMax, movimientos = movimientos ++ compañero.movimientos)

    }

    def morite() = copy(estado = Muerto)

    def quedateInconsciente() = raza.meQuedeInconsciente(this).copy(estado = Inconsciente)

    def cortarCola() = copy(raza = raza.cortarCola, ki = raza.kiLuegoDeCortarCola(this), kiMax = raza.disminuirKiMax(this))

    def transformateEnMono = copy(kiMax = raza.aumentarKiMax(this), ki = kiMax)
  }

  trait Estado {}

  case object Inconsciente extends Estado
  case object Muerto extends Estado
  case object Normal extends Estado
  case class NiUnaMenos(turnos: Int = 1) extends Estado
  
