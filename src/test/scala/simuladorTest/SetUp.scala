package simuladorTest

import simulador._
import simulador.ArenaDeCell._
import org.scalatest._
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.collection.immutable.List
import scala.collection.immutable.Set
import scala.collection.GenTraversableOnce
import simulador.TuplasUtils._

object SetUp {
  val bulma = Guerrero(Humano, 100, 50000, List(), Set(), Normal)
  val mrSatan = Guerrero(Humano, 1000, 50000, List(), Set(), Normal)
  val androide18 = Guerrero(Androide, 1000, 50000, List(), Set(), Normal)
  val dabura = Guerrero(Monstruo, 1000, 50000, List(), Set(), Normal)
  val gokuSS3 = Guerrero(Saiyajin(true), 20000, 50000, List(FotoDeLaLuna), Set(), SSJ(3))
  val gokuNormal = Guerrero(Saiyajin(true), 1000, 50000, List(FotoDeLaLuna), Set(), Normal)
  val gohanSinCola = Guerrero(Saiyajin(false), 1000, 50000, List(FotoDeLaLuna), Set(), Normal)
  val bardockSinFoto = Guerrero(Saiyajin(true), 1000, 50000, List(), Set(), Normal)
  val humanoConItemRomo = Guerrero(Humano, 1000, 50000, List(Roma), Set(), Normal)
  val yajirobe = Guerrero(Humano, 1000, 5000, List(Filosa), Set(), Normal)
  val ramboConMunicion = Guerrero(Humano, 2000, 3000, List(Fuego(100)), Set(), Normal)
  val dende = Guerrero(Namekusein, 100, 200, List(), Set(), Normal)
  val karin = Guerrero(Humano, 10, 100, List(SemillaDelErmitaño), Set(), Normal)
  val monstruoFeo = Guerrero(Monstruo, 1000, 10000, List(), Set(), Normal)
  val esferas7: List[Item] = List.range(1, 8).map { n => EsferasDelDragon(n) }
  val krilin = Guerrero(Humano, 5000, 10000, esferas7, Set(), Normal)
  val kingCold = Guerrero(Monstruo, 5000, 10000, List(), Set(), Normal)
  val monoGigante = Guerrero(Saiyajin(true), 5000, 10000, List(), Set(), MonoGigante)
  val inconsciente = Guerrero(Monstruo, 5000, 10000, List(), Set(), Inconsciente)
  val cell = Guerrero(Monstruo, 10000, 50000, List(), Set(explotar), Normal)
  val majinBuu = Guerrero(Monstruo, 20000, 70000, List(), Set(onda(100)), Normal)
  val maestroRoshi = Guerrero(Humano, 1000, 2000, List(), Set(onda(1000), DejarseFajar), Normal)

  val kamehameha: onda = onda(500)
  val gokuMovimientos: Set[Movimiento] = Set(muchosGolpesNinja, kamehameha, genkidama, Fusion(krilin), DejarseFajar, cargarKi, convertirseEnMonoGigante, ConvertirseEnSS)
  val goku: Guerrero = Guerrero(Saiyajin(true), 1000, 50000, List(FotoDeLaLuna, SemillaDelErmitaño), gokuMovimientos, Normal)
  val vegeta: Guerrero = goku.copy(movimientos = gokuMovimientos - (Fusion(krilin), kamehameha))
  val chiChi = Guerrero(Humano, 1, 25, List(), Set(muchosGolpesNinja), Normal)
  val kami = Guerrero(Humano, 1, 25, List(), Set(), Normal)
  val piccolo = Guerrero(Namekusein, 1500, 3000, List(), gokuMovimientos, Normal)
  val freezer = Guerrero(Monstruo, 2000, 3000, List(), Set(kamehameha, onda(100)), Normal)
  val yamcha = Guerrero(Humano, 2000, 2000, List(Filosa, SemillaDelErmitaño), Set(UsarItem(Filosa), UsarItem(SemillaDelErmitaño)), Normal)
  val cellJr = Guerrero(Monstruo, 2000, 2000, List(), Set(onda(300)), Normal)
  val humano1701KiKamehameha = Guerrero(Humano, 1701, 3000, List(), Set(onda(400)), Normal)

  val magiaDende = (luchadores: Luchadores) => {
    (luchadores._1.aumentarKi(500), luchadores._2.disminuirKi(200))
  }

  val magiaLoca = (luchadores: Luchadores) => {
    (luchadores._1.quedateInconsciente(), luchadores._2.removerItem(SemillaDelErmitaño))
  }

  val androide17 = Guerrero(Androide, 200, 500, List(), Set(explotar, DejarseFajar, Magia(magiaLoca)), Normal)

  val superMagia = (luchadores: Luchadores) => {
    (luchadores._1.recuperarMaxPotencial(), luchadores._2.disminuirKi(100000))
  }

  val rojelioCargaKi = Guerrero(Namekusein, 1001, 1500, List(), Set(Magia(superMagia), DejarseFajar, onda(400)), Normal)

  val formaDigerirDeCell = (luchadores: Luchadores) => {
    (luchadores._2.raza) match {
      case Androide => luchadores._1.movimientos ++ luchadores._2.movimientos
      case _        => luchadores._1.movimientos
    }
  }

  val formaDigerirDeMajinBuu = (luchadores: Luchadores) => {
    luchadores._2.movimientos
  }
}