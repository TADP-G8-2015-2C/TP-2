package simulador

import scala.collection.immutable.List
import scala.collection.immutable.Set
import scala.collection.GenTraversableOnce

object ArenaDeCell {

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
  //Object class no serían??
  case object Inconsciente extends Estado
  case object Muerto extends Estado
  case object Normal extends Estado

  trait Item {}
  //esta parte quedo demasiado funcional pn, habría que delegar un poco y ver que se puede ahcer para no repetir tanta logica
  case class UsarItem(item: Item) extends Movimiento {
    def apply(luchadores: Luchadores) = {
      if (!luchadores._1.poseeItem(item)) {
        luchadores
      } else {
        (item, luchadores._1.raza, luchadores._2.get.raza) match {
          case (Roma(), _, Androide(_))                     => luchadores
          case (Roma(), _, _) if luchadores._2.get.ki < 300 => (luchadores._1, luchadores._2.map(defe => defe quedateInconsciente))
          case (Filosa(), _, Saiyajin(true, _, _))          => (luchadores._1, luchadores._2.map(defe => defe cortarCola))
          case (Filosa(), _, _)                             => (luchadores._1, luchadores._2.map(defe => defe disminuirKi (luchadores._1.ki / 100)))
          case (Fuego(cant), _, _) if cant <= 0             => luchadores
          case (Fuego(cant), _, Humano()) => (luchadores._1.disminuirMunicion(cant),
            luchadores._2.map(defe => defe disminuirKi (20)))
          case (Fuego(cant), _, Namekusein()) if luchadores._2.get.estado == Inconsciente =>
            (luchadores._1.disminuirMunicion(cant), luchadores._2.map(defe => defe disminuirKi (10)))
          case (Fuego(cant), _, _)          => (luchadores._1.disminuirMunicion(cant), luchadores._2)
          case (SemillaDelErmitaño(), _, _) => (luchadores._1.recuperarMaxPotencial(), luchadores._2)
        }
      }
    }
  }

  case class Roma() extends Item {}
  case class Filosa() extends Item {}
  case class Fuego(cant: Int = 0) extends Item {}
  case class SemillaDelErmitaño() extends Item {}
  case class SieteEsferasDelDragon() extends Item {}
  case class FotoDeLaLuna() extends Item {}

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

  abstract class Raza {
    def cortarCola = this
    def kiLuegoDeCortarCola(unGuerrero: Guerrero) = unGuerrero.ki
    def aumentarKiMax(unGuerrero: Guerrero) = unGuerrero.kiMax
    def disminuirKiMax(unGuerrero: Guerrero) = unGuerrero.kiMax
    def subirDeNivel(unGuerrero: Guerrero) = this
    def meQuedeInconsciente(unGuerrero: Guerrero) = unGuerrero
  }

  case class Monstruo() extends Raza {}
  case class Humano() extends Raza {}
  case class Androide(bateria: Int = 0) extends Raza {}
  case class Namekusein() extends Raza {}
  case class Fusionado(guerreroOriginal: Guerrero) extends Raza {}
  case class Saiyajin(cola: Boolean, nivel: Int = 0, monoGigante: Boolean = false) extends Raza {

    override def subirDeNivel(guerrero: Guerrero) = {
      if (guerrero.ki >= guerrero.kiMax) {
        copy(nivel = nivel + 1)
      } else {
        this
      }
    }

    override def cortarCola = copy(cola = false, monoGigante = false)
    override def kiLuegoDeCortarCola(unGuerrero: Guerrero) = 1 //TODO lo del MonoGigante como tratarlo
    override def aumentarKiMax(unGuerrero: Guerrero) = unGuerrero.kiMax * 3
    override def disminuirKiMax(unGuerrero: Guerrero) = unGuerrero.kiMax / 3

    override def meQuedeInconsciente(unGuerrero: Guerrero) = {
      unGuerrero.dejarDeSerSS()

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