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

  val bulma = Guerrero(Humano(), 100,50000 ,List(), Set(), Normal)
  val mrSatan = Guerrero(Humano(), 1000, 50000, List(), Set(), Normal)
  val goku = Guerrero(Saiyajin(true, 3), 20000, 50000, List(), Set(), Normal)
  val androide18 = Guerrero(Androide(1000), 0, 50000, List(), Set(), Normal)
  val humanoConItemRomo = Guerrero(Humano(), 1000, 50000, List(Roma()), Set(), Normal)
  val yajirobe = Guerrero(Humano(), 1000, 5000, List(Filosa()), Set(), Normal)
  val ramboConMunicion = Guerrero(Humano(), 2000, 3000, List(Fuego(100)), Set(), Normal)
  val dende = Guerrero(Namekusein(), 100, 200, List(), Set(), Normal)
  val karin = Guerrero(Humano(), 10, 100, List(SemillaDelErmitaño()), Set(), Normal)
  val monstruoFeo = Guerrero(Monstruo(), 1000, 10000, List(), Set(), Normal)
  val krilin = Guerrero(Humano(), 5000, 10000, List(SieteEsferasDelDragon()), Set(), Normal)
  
  val magiaDende = (luchadores: Luchadores) => {
    (luchadores._1.aumentarKi(500), Option(luchadores._2.get.disminuirKi(200)))
  }
  
  val magiaLoca = (luchadores: Luchadores) => {
    (luchadores._1.quedateInconsciente(), Option(luchadores._2.get.removerItem(SemillaDelErmitaño())))
  }
  
  val superMagia = (luchadores: Luchadores) => {
    (luchadores._1.recuperarMaxPotencial(), Option(luchadores._2.get.disminuirKi(100000)))
  }
  
  //Test cargarKi
  "mrSatan" should "cargarKi y subir 100 de ki por ser Guerrero" in {

    assertResult(1100) {
      cargarKi(mrSatan, None)._1.ki
    }
  }

  "goku" should "cargarKi y subir ki en  150 * nivel de SS por ser Saiyajin" in {

    assert(cargarKi(goku, None)._1.ki === 20450)
  }

  "androide18" should "cargarKi y no subir nada (seguir en 0) por ser un Androide" in {

    assert(cargarKi(androide18, None)._1.ki === 0)
  }

  //Test usarItem Romo
  "mrSatan" should "no hacer nada  cuando useItem Romo ya que no lo tiene en su inventario" in {

    val humanito = Guerrero(Humano(), 1, 50000, List(), Set(), Normal)
    val luchadoresLuegoDeUsarItemRoma = UsarItem(Roma())((mrSatan, Option(humanito)))

    assert(luchadoresLuegoDeUsarItemRoma._1 === mrSatan && luchadoresLuegoDeUsarItemRoma._2 === Option(humanito))
  }

  "androide18" should "queda igual porque los androides no se modifican al recibir ataque de item Romo" in {

    val luchadoresLuegoDeUsarItemRoma = UsarItem(Roma())((humanoConItemRomo, Option(androide18)))

    assert(luchadoresLuegoDeUsarItemRoma._1 === humanoConItemRomo && luchadoresLuegoDeUsarItemRoma._2 === Option(androide18))
  }

  "bulma" should "quedar iconsciente al recibir ataque con item Romo y tener menos de 300 de ki" in {

    val luchadoresLuegoDeUsarItemRoma = UsarItem(Roma())((humanoConItemRomo, Option(bulma)))

    assert(luchadoresLuegoDeUsarItemRoma._2.get.estado === Inconsciente)
  }

  "bulma" should "quedar nomal al recibir ataque con item Romo de alguien que no posee dicho item aunque tiene menos de 300 de ki" in {

    val humanoSinItemRomo = Guerrero(Humano(), 1000, 50000, List(), Set(), Normal)
    val luchadoresLuegoDeUsarItemRoma = UsarItem(Roma())((humanoSinItemRomo, Option(bulma)))

    assert(luchadoresLuegoDeUsarItemRoma._2.get.estado === Normal)
  }
  
  
  //Test usarItem Filoso
  "yajirobe" should "disminuir en 10 (1000/100) el ki de mrSatan al atacarlo con arma filosa" in {
    
    val luchadoresLuegoDeUsarItemFiloso = UsarItem(Filosa())((yajirobe, Option(mrSatan)))
    
    assert(luchadoresLuegoDeUsarItemFiloso._2.get.ki === 990)
  }
  
  //TODO falta ver lo del mono gigante.
  "yajirobe" should "cortar cola de goku y quedar en 1 de ki por ser este un saiyajin con cola y darle con arma filosa" in {
    
    val luchadoresLuegoDeUsarItemFiloso = UsarItem(Filosa())((yajirobe, Option(goku)))
    
    assert(luchadoresLuegoDeUsarItemFiloso._2.get.raza === Saiyajin(false,3) && luchadoresLuegoDeUsarItemFiloso._2.get.ki === 1)
  }
  
  //Test usarItem Fuego
  "rambo" should "no hacer nada a ninguna raza porque no tiene munición en su arma de fuego" in {
  
    val ramboSinBalas = Guerrero(Humano(), 2000, 3000, List(Fuego(0)), Set(), Normal)
    
    val luchadoresLuegoDeUsarItemFuego = UsarItem(Fuego(0))((ramboSinBalas, Option(humanoConItemRomo)))
    
    assert(luchadoresLuegoDeUsarItemFuego._2.get.ki === 1000)
  }
  
  "rambo" should "hacer 20 de danio raza Humana y disminuir en 1 su municion en el arma" in {
  
    val luchadoresLuegoDeUsarItemFuego = UsarItem(Fuego(100))((ramboConMunicion, Option(humanoConItemRomo)))
    
    val armaFuegoDisminuyoMunicion = luchadoresLuegoDeUsarItemFuego._1.items.find { item => item === Fuego(99) }
    
    assert(luchadoresLuegoDeUsarItemFuego._2.get.ki === 980 && armaFuegoDisminuyoMunicion.isEmpty === false)
  }
  
  "rambo" should "hacer 10 de danio raza Namekusein inconsciente y disminuir en 1 su municion en el arma" in {
  
    val dendeInconsciente = Guerrero(Namekusein(), 100, 200, List(), Set(), Inconsciente)
    val luchadoresLuegoDeUsarItemFuego = UsarItem(Fuego(100))((ramboConMunicion, Option(dendeInconsciente)))
    
    val armaFuegoDisminuyoMunicion = luchadoresLuegoDeUsarItemFuego._1.items.find { item => item === Fuego(99) }
    
    assert(luchadoresLuegoDeUsarItemFuego._2.get.ki === 90 && armaFuegoDisminuyoMunicion.isEmpty === false)
  }
  
  "rambo" should "no hacer danio a Namekusein normal y disminuir en 1 su municion en el arma" in {
  
    val luchadoresLuegoDeUsarItemFuego = UsarItem(Fuego(100))((ramboConMunicion, Option(dende)))
    
    val armaFuegoDisminuyoMunicion = luchadoresLuegoDeUsarItemFuego._1.items.find { item => item === Fuego(99) }
    
    assert(luchadoresLuegoDeUsarItemFuego._2.get.ki === 100 && armaFuegoDisminuyoMunicion.isEmpty === false)
  }
  
  //Test semilla de ermitaño
  "karin" should "recuperarse al máximo el ki  por tener semillas del Ermitanio" in {
    
    val luchadoresLuegoDeUsarItemSemilla = UsarItem(SemillaDelErmitaño())((karin, Option(goku)))
    
    assert(luchadoresLuegoDeUsarItemSemilla._1.ki === luchadoresLuegoDeUsarItemSemilla._1.kiMax)
  }
  
  //Test Magia
  "dende" should "aplicar una magia dada, ya que es Namekusein" in {
    
    val luchadoresLuegoDeMagia = Magia(magiaDende)(dende, Option(mrSatan))
    
    assert(luchadoresLuegoDeMagia._1.ki === 200 && luchadoresLuegoDeMagia._2.get.ki === 800)
    
  }
  
  "mostruoFeo" should "aplicar una magia dada, ya que es Monstruo" in {
     
    val luchadoresLuegoDeMagia = Magia(magiaLoca)(monstruoFeo, Option(karin))
    
    val tieneItemSemillaDelErmitaño = luchadoresLuegoDeMagia._2.get.items.find { item => item === SemillaDelErmitaño() }
    
    assert(luchadoresLuegoDeMagia._1.estado === Inconsciente && tieneItemSemillaDelErmitaño.isEmpty === true)
  }
  
  "krilin" should "aplicar magia dado que tiene las 7 esferas!" in {
    
    val luchadoresLuegoDeMagia = Magia(superMagia)(krilin, Option(goku))
    
    assert(luchadoresLuegoDeMagia._1.ki === 10000 && luchadoresLuegoDeMagia._2.get.ki === 0)
  }
  
  "mrSatan" should "no hacer magia, ya que es un simple humano sin esferas en su haber" in {
    
    val luchadoresLuegoDeMagia = Magia(superMagia)(mrSatan, Option(goku))
    
    assert(luchadoresLuegoDeMagia._1.ki === 1000 && luchadoresLuegoDeMagia._2.get.ki === 20000)
  }
  
}