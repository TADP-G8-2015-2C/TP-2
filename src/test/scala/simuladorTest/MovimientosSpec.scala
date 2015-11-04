package simuladorTest

import simulador._
import simulador.ArenaDeCell._
import org.scalatest._
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.collection.immutable.List
import scala.collection.immutable.Set
import scala.collection.GenTraversableOnce

class MovimientosSpec extends FlatSpec with Matchers {

  val bulma = Guerrero(Humano(), 100, 50000, List(), Set(), Normal)
  val mrSatan = Guerrero(Humano(), 1000, 50000, List(), Set(), Normal)
  val androide18 = Guerrero(Androide(1000), 0, 50000, List(), Set(), Normal)
  val dabura = Guerrero(Monstruo(), 1000, 50000, List(), Set(), Normal)
  val goku = Guerrero(Saiyajin(true, 3), 20000, 50000, List(), Set(), Normal)
  val gokuNormal = Guerrero(Saiyajin(true, 1), 1000, 50000, List(), Set(), Normal)
  val humanoConItemRomo = Guerrero(Humano(), 1000, 50000, List(Roma()), Set(), Normal)
  val yajirobe = Guerrero(Humano(), 1000, 5000, List(Filosa()), Set(), Normal)
  val ramboConMunicion = Guerrero(Humano(), 2000, 3000, List(Fuego(100)), Set(), Normal)
  val dende = Guerrero(Namekusein(), 100, 200, List(), Set(), Normal)
  val karin = Guerrero(Humano(), 10, 100, List(SemillaDelErmitaño()), Set(), Normal)
  val monstruoFeo = Guerrero(Monstruo(), 1000, 10000, List(), Set(), Normal)
  val krilin = Guerrero(Humano(), 5000, 10000, List(SieteEsferasDelDragon()), Set(), Normal)

  val magiaDende = (luchadores: Luchadores) => {
    (luchadores._1.aumentarKi(500), luchadores._2.disminuirKi(200))
  }

  val magiaLoca = (luchadores: Luchadores) => {
    (luchadores._1.quedateInconsciente(), luchadores._2.removerItem(SemillaDelErmitaño()))
  }

  val superMagia = (luchadores: Luchadores) => {
    (luchadores._1.recuperarMaxPotencial(), luchadores._2.disminuirKi(100000))
  }
  //test genkidama
  "goku" should "intenta lanzar genkidama sin dejarse fajar" in {
    val (gokuAfter, mrSatanAfter) = genkidama(gokuNormal, mrSatan)
    assertResult((1000, 1000)) {
      (gokuAfter.ki, mrSatanAfter.ki)

    }
  } 
      "goku" should "lanzar genkidama dejandose fajar 2 turnos" in {
    val (gokuAfter, mrSatanAfter) = genkidama(DejarseFajar(DejarseFajar(gokuNormal, mrSatan)))
    assertResult((1000, 900,NiUnaMenos(2))) {
      (gokuAfter.ki, mrSatanAfter.ki,gokuAfter.estado)
    } 
      }

 
  //Test ondas
  "bulma" should "tirar manseko sin hacer nada porque no tiene kiSufieciente" in {
    val masenkoAfter = onda(105)(bulma, mrSatan)
    assertResult((100, 1000)) {
      (masenkoAfter._1.ki, masenkoAfter._2.ki)
    }
  }
  "androide18" should "tirar manseko sin hacer nada porque no tiene kiSufieciente" in {
    val (androide18After, mrSatanAfter) = onda(1005)(androide18, mrSatan)
    assertResult((androide18, mrSatan)) {
      (androide18After, mrSatanAfter)
    }
  }

  "androide18" should "tirar manseko a humano gasta el doble" in {
    val (androide18After, mrSatanAfter) = onda(300)(androide18, mrSatan)
    assertResult((700, 400)) {
      (androide18After.raza.bateria, mrSatanAfter.ki)
    }
  }

  "androide18" should "tirar manseko a monstruo gasta la mitad" in {
    val (androide18After, daburaAfter) = onda(300)(androide18, dabura)
    assertResult((700, 850)) {
      (androide18After.raza.bateria, daburaAfter.ki)
    }
  }

  "cualquiera" should "tirar manseko a monstruo gasta la mitad" in {
    val (mrSatanAfter, daburaAfter) = onda(300)(mrSatan, dabura)
    assertResult((700, 850)) {
      (mrSatanAfter.ki, daburaAfter.ki)
    }
  }

  "cualquiera" should "tirar manseko a cualquiera el doble" in {
    val (mrSatanAfter, bulmaAfter) = onda(30)(mrSatan, bulma)
    assertResult((970, 40)) {
      (mrSatanAfter.ki, bulmaAfter.ki)
    }
  }

  //Test cargarKi
  "mrSatan" should "cargarKi y subir 100 de ki por ser Guerrero" in {

    assertResult(1100) {
      cargarKi(mrSatan, krilin)._1.ki
    }
  }

  "goku" should "cargarKi y subir ki en  150 * nivel de SS por ser Saiyajin" in {

    assert(cargarKi(goku, krilin)._1.ki === 20450)
  }

  "androide18" should "cargarKi y no subir nada (seguir en 0) por ser un Androide" in {

    assert(cargarKi(androide18, krilin)._1.ki === 0)
  }

  //Test usarItem Romo
  "mrSatan" should "no hacer nada  cuando useItem Romo ya que no lo tiene en su inventario" in {

    val humanito = Guerrero(Humano(), 1, 50000, List(), Set(), Normal)
    val luchadoresLuegoDeUsarItemRoma = UsarItem(Roma())((mrSatan, humanito))

    assert(luchadoresLuegoDeUsarItemRoma._1 === mrSatan && luchadoresLuegoDeUsarItemRoma._2 === humanito)
  }

  "androide18" should "queda igual porque los androides no se modifican al recibir ataque de item Romo" in {

    val luchadoresLuegoDeUsarItemRoma = UsarItem(Roma())((humanoConItemRomo, androide18))

    assert(luchadoresLuegoDeUsarItemRoma._1 === humanoConItemRomo && luchadoresLuegoDeUsarItemRoma._2 === androide18)
  }

  "bulma" should "quedar iconsciente al recibir ataque con item Romo y tener menos de 300 de ki" in {

    val luchadoresLuegoDeUsarItemRoma = UsarItem(Roma())((humanoConItemRomo, bulma))

    assert(luchadoresLuegoDeUsarItemRoma._2.estado === Inconsciente)
  }

  "bulma" should "quedar nomal al recibir ataque con item Romo de alguien que no posee dicho item aunque tiene menos de 300 de ki" in {

    val humanoSinItemRomo = Guerrero(Humano(), 1000, 50000, List(), Set(), Normal)
    val luchadoresLuegoDeUsarItemRoma = UsarItem(Roma())((humanoSinItemRomo, bulma))

    assert(luchadoresLuegoDeUsarItemRoma._2.estado === Normal)
  }

  //Test usarItem Filoso
  "yajirobe" should "disminuir en 10 (1000/100) el ki de mrSatan al atacarlo con arma filosa" in {

    val luchadoresLuegoDeUsarItemFiloso = UsarItem(Filosa())((yajirobe, mrSatan))

    assert(luchadoresLuegoDeUsarItemFiloso._2.ki === 990)
  }

  //TODO falta ver lo del mono gigante.
  "yajirobe" should "cortar cola de goku y quedar en 1 de ki por ser este un saiyajin con cola y darle con arma filosa" in {

    val luchadoresLuegoDeUsarItemFiloso = UsarItem(Filosa())((yajirobe, goku))

    assert(luchadoresLuegoDeUsarItemFiloso._2.raza === Saiyajin(false, 3) && luchadoresLuegoDeUsarItemFiloso._2.ki === 1)
  }

  //Test usarItem Fuego
  "rambo" should "no hacer nada a ninguna raza porque no tiene munición en su arma de fuego" in {

    val ramboSinBalas = Guerrero(Humano(), 2000, 3000, List(Fuego(0)), Set(), Normal)

    val luchadoresLuegoDeUsarItemFuego = UsarItem(Fuego(0))((ramboSinBalas, humanoConItemRomo))

    assert(luchadoresLuegoDeUsarItemFuego._2.ki === 1000)
  }

  "rambo" should "hacer 20 de danio raza Humana y disminuir en 1 su municion en el arma" in {

    val luchadoresLuegoDeUsarItemFuego = UsarItem(Fuego(100))((ramboConMunicion, humanoConItemRomo))

    val armaFuegoDisminuyoMunicion = luchadoresLuegoDeUsarItemFuego._1.items.find { item => item === Fuego(99) }

    assert(luchadoresLuegoDeUsarItemFuego._2.ki === 980 && armaFuegoDisminuyoMunicion.isEmpty === false)
  }

  "rambo" should "hacer 10 de danio raza Namekusein inconsciente y disminuir en 1 su municion en el arma" in {

    val dendeInconsciente = Guerrero(Namekusein(), 100, 200, List(), Set(), Inconsciente)
    val luchadoresLuegoDeUsarItemFuego = UsarItem(Fuego(100))((ramboConMunicion, dendeInconsciente))

    val armaFuegoDisminuyoMunicion = luchadoresLuegoDeUsarItemFuego._1.items.find { item => item === Fuego(99) }

    assert(luchadoresLuegoDeUsarItemFuego._2.ki === 90 && armaFuegoDisminuyoMunicion.isEmpty === false)
  }

  "rambo" should "no hacer danio a Namekusein normal y disminuir en 1 su municion en el arma" in {

    val luchadoresLuegoDeUsarItemFuego = UsarItem(Fuego(100))((ramboConMunicion, dende))

    val armaFuegoDisminuyoMunicion = luchadoresLuegoDeUsarItemFuego._1.items.find { item => item === Fuego(99) }

    assert(luchadoresLuegoDeUsarItemFuego._2.ki === 100 && armaFuegoDisminuyoMunicion.isEmpty === false)
  }

  //Test semilla de ermitaño
  "karin" should "recuperarse al máximo el ki  por tener semillas del Ermitanio" in {

    val luchadoresLuegoDeUsarItemSemilla = UsarItem(SemillaDelErmitaño())((karin, goku))

    assert(luchadoresLuegoDeUsarItemSemilla._1.ki === luchadoresLuegoDeUsarItemSemilla._1.kiMax)
  }

  //Test Magia
  "dende" should "aplicar una magia dada, ya que es Namekusein" in {

    val luchadoresLuegoDeMagia = Magia(magiaDende)(dende, mrSatan)

    assert(luchadoresLuegoDeMagia._1.ki === 200 && luchadoresLuegoDeMagia._2.ki === 800)

  }

  "mostruoFeo" should "aplicar una magia dada, ya que es Monstruo" in {

    val luchadoresLuegoDeMagia = Magia(magiaLoca)(monstruoFeo, karin)

    val tieneItemSemillaDelErmitaño = luchadoresLuegoDeMagia._2.items.find { item => item === SemillaDelErmitaño() }

    assert(luchadoresLuegoDeMagia._1.estado === Inconsciente && tieneItemSemillaDelErmitaño.isEmpty === true)
  }

  "krilin" should "aplicar magia dado que tiene las 7 esferas!" in {

    val luchadoresLuegoDeMagia = Magia(superMagia)(krilin, goku)

    assert(luchadoresLuegoDeMagia._1.ki === 10000 && luchadoresLuegoDeMagia._2.ki === 0)
  }

  "mrSatan" should "no hacer magia, ya que es un simple humano sin esferas en su haber" in {

    val luchadoresLuegoDeMagia = Magia(superMagia)(mrSatan, goku)

    assert(luchadoresLuegoDeMagia._1.ki === 1000 && luchadoresLuegoDeMagia._2.ki === 20000)
  }

}