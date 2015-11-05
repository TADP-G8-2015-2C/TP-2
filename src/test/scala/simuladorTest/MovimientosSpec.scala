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

  val bulma = Guerrero(Humano, 100, 50000, List(), Set(), Normal)
  val mrSatan = Guerrero(Humano, 1000, 50000, List(), Set(), Normal)
  val androide18 = Guerrero(Androide, 1000, 50000, List(), Set(), Normal)
  val dabura = Guerrero(Monstruo, 1000, 50000, List(), Set(), Normal)
  val gokuSS3 = Guerrero(Saiyajin(true, 3), 20000, 50000, List(FotoDeLaLuna), Set(), Normal)
  val gokuNormal = Guerrero(Saiyajin(true, 1), 1000, 50000, List(FotoDeLaLuna), Set(), Normal)
  val gohanSinCola = Guerrero(Saiyajin(false, 1), 1000, 50000, List(FotoDeLaLuna), Set(), Normal)
  val bardockSinFoto = Guerrero(Saiyajin(true, 1), 1000, 50000, List(), Set(), Normal)
  val humanoConItemRomo = Guerrero(Humano, 1000, 50000, List(Roma), Set(), Normal)
  val yajirobe = Guerrero(Humano, 1000, 5000, List(Filosa), Set(), Normal)
  val ramboConMunicion = Guerrero(Humano, 2000, 3000, List(Fuego(100)), Set(), Normal)
  val dende = Guerrero(Namekusein, 100, 200, List(), Set(), Normal)
  val karin = Guerrero(Humano, 10, 100, List(SemillaDelErmitaño), Set(), Normal)
  val monstruoFeo = Guerrero(Monstruo, 1000, 10000, List(), Set(), Normal)
  val esferas7: List[Item] = List.range(1, 8).map { n => EsferasDelDragon(n) }
  val krilin = Guerrero(Humano, 5000, 10000, esferas7, Set(), Normal)
  val kingCold = Guerrero(Monstruo, 5000, 10000, List(), Set(), Normal)
  val monoGigante = Guerrero(Saiyajin(true, 1, true), 5000, 10000, List(), Set(), Normal)
  val inconsciente = Guerrero(Monstruo, 5000, 10000, List(), Set(), Inconsciente)
  val cell = Guerrero(Monstruo, 10000, 50000, List(), Set(explotar), Normal)
  val majinBuu = Guerrero(Monstruo, 20000, 70000, List(), Set(onda(100)), Normal)
  val maestroRoshi = Guerrero(Humano, 1000, 2000, List(), Set(onda(1000), DejarseFajar), Normal)
  
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
  
  val formaDigerirDeCell = (luchadores: Luchadores) => {
    (luchadores._2.raza) match {
      case Androide => luchadores._1.movimientos ++ luchadores._2.movimientos
      case _ => luchadores._1.movimientos
    }
  }
  
  val formaDigerirDeMajinBuu = (luchadores: Luchadores) => {
     luchadores._2.movimientos
  }
  //Fusion
  "dabura" should "no se puede fusionar con gokuNormal" in {
    assertResult(dabura.raza) {
      (Fusion(gokuNormal)(dabura, mrSatan))._1.raza // Magia(magiaDende)(dende, mrSatan)
    }
  }
  "gokuNormal" should "no se puede fusionar con dabura" in {
    assertResult(gokuNormal.raza) {
      (Fusion(dabura)(gokuNormal, mrSatan))._1.raza
    }
  }
  "dabura" should "no se puede fusionar con androide18" in {
    assertResult(dabura.raza) {
      (Fusion(androide18)(dabura, gokuNormal))._1.raza
    }
  }
  "bulma" should "se fusiona con dende y cambia de especie" in {
    assertResult(Fusionado(bulma)) {
      (Fusion(dende)(bulma, androide18))._1.raza
    }
  }
  "bulma" should "se fusiona con dende y combina movimientos" in {
    assertResult(bulma.movimientos ++ dende.movimientos) {
      (Fusion(dende)(bulma, androide18))._1.movimientos
    }
  }
  "bulma" should "se fusiona con dende y combina ambos ki" in {
    assertResult(bulma.ki + dende.ki) {
      (Fusion(dende)(bulma, androide18))._1.ki
    }
  }
  "bulma" should "se fusiona con dende y combina ambos kiMax" in {
    assertResult(bulma.kiMax + dende.kiMax) {
      (Fusion(dende)(bulma, androide18))._1.kiMax
    }
  }
  //ConvertirseEnSS
  "kingCold" should "no puede convertirse en ss porque no lo es" in {
    assertResult(kingCold.raza) {
      ConvertirseEnSS(kingCold, mrSatan)._1.raza
    }
  }

  "gokuNormal" should "no puede convertirse en ss no tiene el poder suficiente" in {
    assertResult(gokuNormal.raza) {
      ConvertirseEnSS(gokuNormal, mrSatan)._1.raza
    }
  }
  "gokuNormal" should "carga ki y se convierte en ss " in {
    assertResult(Saiyajin(true, 2)) {
      ConvertirseEnSS(gokuNormal.aumentarKi(25000), mrSatan)._1.raza
    }
  }
  "gokuNormal" should "se convierte en ss y aumenta su kiMax" in {
    assertResult(gokuNormal.kiMax * (1 + 1) * 5) {
      ConvertirseEnSS(gokuNormal.aumentarKi(25000), mrSatan)._1.kiMax
    }
  }
  //dejar de ser ss
  "kingCold" should "no puede dejar de ser ss porque no lo es" in {
    assertResult(kingCold.raza) {
      kingCold.dejarDeSerSS().raza
    }
  }
  "gokuSS3" should "deja de ser ss" in {
    assertResult(Saiyajin(true, 1)) {
      gokuSS3.dejarDeSerSS().raza
    }
  }
  "gokuSS3" should "deja de ser ss y se le modifica bien su ki" in {
    assertResult(5000) {
      gokuSS3.dejarDeSerSS().ki
    }
  }
  "gokuSS3" should "deja de ser ss y se le modifica bien su kiMax" in {
    assertResult(5000) {
      gokuSS3.dejarDeSerSS().kiMax
    }
  }
  //Convertirse en Mono
  "kingCold" should "no puede converstirse en Mono por no ser Saiyajin" in {
    val (kingColdAfter, mrSatanAfter) = convertirseEnMonoGigante(kingCold, mrSatan)
    assertResult(Monstruo) {
      (kingColdAfter.raza)
    }
  }

  "gohanSinCola" should "no puede converstirse en Mono por no tener cola" in {
    val (gohanSinColaAfter, mrSatanAfter) = convertirseEnMonoGigante(gohanSinCola, mrSatan)
    assertResult(gohanSinCola.raza) {
      (gohanSinColaAfter.raza)
    }
  }

  "bardockSinFoto" should "no puede converstirse en Mono por no tener foto" in {
    val (bardockSinFotoAfter, mrSatanAfter) = convertirseEnMonoGigante(bardockSinFoto, mrSatan)
    assertResult(bardockSinFoto.raza) {
      (bardockSinFotoAfter.raza)
    }
  }

  "gokuNormal" should "se convierte en Mono" in {
    val (gokuNormalAfter, mrSatanAfter) = convertirseEnMonoGigante(gokuNormal, mrSatan)
    assertResult(Saiyajin(true, 1, true)) {
      (gokuNormalAfter.raza)
    }
  }
  "gokuNormal" should "se convierte en Mono y su ki se recupera a su viejo KiMax" in {
    val (gokuNormalAfter, mrSatanAfter) = convertirseEnMonoGigante(gokuNormal, mrSatan)
    assertResult(gokuNormal.kiMax) {
      (gokuNormalAfter.ki)
    }
  }
  "gokuNormal" should "se convierte en Mono y se modifica kiMax" in {
    val (gokuNormalAfter, mrSatanAfter) = convertirseEnMonoGigante(gokuNormal, mrSatan)
    assertResult(gokuNormal.kiMax * 3) {
      (gokuNormalAfter.kiMax)
    }
  }

  "gokuSS3" should "se convierte en Mono" in {
    val (gokuSS3After, mrSatanAfter) = convertirseEnMonoGigante(gokuSS3, mrSatan)
    assertResult(Saiyajin(true, 1, true)) {
      (gokuSS3After.raza)
    }
  }

  "gokuSS3" should "se convierte en Mono y modifica kiMax" in {
    val (gokuSS3After, mrSatanAfter) = convertirseEnMonoGigante(gokuSS3, mrSatan)
    assertResult((gokuSS3.kiMax / (5 * (3 - 1))) * 3) {
      (gokuSS3After.kiMax)
    }
  }

  "gokuSS3" should "se convierte en Mono y modifica ki" in {
    val (gokuSS3After, mrSatanAfter) = convertirseEnMonoGigante(gokuSS3, mrSatan)
    assertResult((gokuSS3.kiMax / (5 * (3 - 1)))) {
      (gokuSS3After.ki)
    }
  }
  
  //Test comerse Al oponente
	"cell" should "comerse a enemigo(androide) y quedarse con sus mov. más los del enemigo, y el enemigo morir" in {
    val luchadoresLuegoDeComer = ComerseAOtro(formaDigerirDeCell)(cell, androide17)
    
    assert(luchadoresLuegoDeComer._1.movimientos.contains(explotar))
    assert(luchadoresLuegoDeComer._1.movimientos.contains(DejarseFajar))
    assert(luchadoresLuegoDeComer._1.movimientos.contains(Magia(magiaLoca)))
    assert(luchadoresLuegoDeComer._1.movimientos.diff(Set(explotar, DejarseFajar, Magia(magiaLoca))) === Set())
    assert(luchadoresLuegoDeComer._2.estado === Muerto)
  }
  
  "cell" should "comerse a enemigo (NO androide) y solo matarlo, sin aprender nada" in {
    
    assert(cell.movimientos.contains(explotar))
    
    val luchadoresLuegoDeComer = ComerseAOtro(formaDigerirDeCell)(cell, maestroRoshi)
    
    assert(luchadoresLuegoDeComer._1.movimientos.diff(Set(explotar)) === Set())
    assert(luchadoresLuegoDeComer._2.estado === Muerto)
  }
  
  "mainBuu" should "comerse a enemigo (cualquiera) y quedarse con sus mov. y olvidar lo anterior" in {
     
     assert(majinBuu.movimientos.contains(onda(100)))
    
     val luchadoresLuegoDeComer = ComerseAOtro(formaDigerirDeMajinBuu)(majinBuu, maestroRoshi)
     
     assert(luchadoresLuegoDeComer._1.movimientos.diff(Set(onda(1000), DejarseFajar)) === Set())
     assert(luchadoresLuegoDeComer._2.estado === Muerto)
  }
  
  "krilin" should "no hacer nada al querer comer a otro, ya que solo los monstruos pueden" in {
    val luchadoresLuegoDeComer = ComerseAOtro(formaDigerirDeCell)(krilin, maestroRoshi)
    
    assert(krilin === luchadoresLuegoDeComer._1 && maestroRoshi === luchadoresLuegoDeComer._2)
  }

  //Test MuchosGolpesNinja  
  "mrSatan" should "sufrir 20 puntos de daño al ser atacado por luchador de mayor ki que el" in {
    assert(muchosGolpesNinja(mrSatan, krilin)._1.ki === 980)
  }
  
  "krilin" should "sufrir 20 puntos de daño al intercambiar golpes ninja con luchador de mayor ki" in {
    assert(muchosGolpesNinja(gokuSS3, krilin)._2.ki === 4980)
  }
  
  "gokuSS3" should "no sufre daño por ser el atacante (no humano) y pegarle a un androide, aunque sea de menor ki" in {
    assert(muchosGolpesNinja(gokuSS3, androide18)._1.ki === 20000)
  }
  
   "krilin" should "Sufre 10 de daño por ser el atacante (humano) y pegarle a un androide, aunque sea de menor ki" in {
    assert(muchosGolpesNinja(krilin, androide18)._1.ki === 4990)
  }
  
  //Test de Explotar
  "monstruoFeo" should "explotar, quedarse en 0 ki, morir y sacar al enemigo el doble del ki perdido" in {
    val luchadoresLuegoDeExplotar = explotar(monstruoFeo, krilin)
    
    assert(luchadoresLuegoDeExplotar._1.ki === 0)
    assert(luchadoresLuegoDeExplotar._1.estado === Muerto)
    assert(luchadoresLuegoDeExplotar._2.ki === 3000)
  }
  
  "androide18" should "explotar, quedarse en 0ki y morir, y sacar al enemigo el triple del ki perdido" in {
    val luchadoresLuegoDeExplotar = explotar(androide18, krilin)
    
    assert(luchadoresLuegoDeExplotar._1.ki === 0)
    assert(luchadoresLuegoDeExplotar._1.estado === Muerto)
    assert(luchadoresLuegoDeExplotar._2.ki === 2000)
  }
  
  "dende" should "quedar en 1 de ki, por ser un namekusein y recibir la exploción" in {
    val luchadoresLuegoDeExplotar = explotar(androide18, dende)
    
    assert(luchadoresLuegoDeExplotar._2.ki === 1)
  }
  
  "krilin" should "hacer nada al querer explotar, por no ser ni monstrou ni androide" in {
    val luchadoresLuegoDeExplotar = explotar(krilin, dende)
    
    assert(luchadoresLuegoDeExplotar._1.ki === 5000)
    assert(luchadoresLuegoDeExplotar._1.estado === Normal)
    assert(luchadoresLuegoDeExplotar._2.ki === 100)
  }
   
   
  //test NiUnaMenos
  "kingCold" should "kingCold se deja fajar y le cambia el estado" in {
    val (kingColdAfter, mrSatanAfter) = DejarseFajar(kingCold, mrSatan)
    assertResult((Normal, NiUnaMenos(1))) {
      (kingCold.estado, kingColdAfter.estado)
    }
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
    assertResult((1000, 900, Normal)) {
      (gokuAfter.ki, mrSatanAfter.ki, gokuAfter.estado)
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
      (androide18After.ki, mrSatanAfter.ki)
    }
  }

  "androide18" should "tirar manseko a monstruo gasta la mitad" in {
    val (androide18After, daburaAfter) = onda(300)(androide18, dabura)
    assertResult((700, 850)) {
      (androide18After.ki, daburaAfter.ki)
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
  "androide18" should "cargarKi y subir 0 de ki por ser Androide" in {
    assertResult(androide18.ki) {
      cargarKi(androide18, bulma)._1.ki
    }
  }

  "goku" should "cargarKi y subir ki en  150 * nivel de SS por ser Saiyajin" in {

    assert(cargarKi(gokuSS3, krilin)._1.ki === 20450)
  }

  "androide18" should "cargarKi y no subir nada (seguir en 0) por ser un Androide" in {

    assert(cargarKi(androide18, krilin)._1.ki === 1000)
  }

  //Test usarItem Romo
  "mrSatan" should "no hacer nada  cuando useItem Romo ya que no lo tiene en su inventario" in {

    val humanito = Guerrero(Humano, 1, 50000, List(), Set(), Normal)
    val luchadoresLuegoDeUsarItemRoma = UsarItem(Roma)((mrSatan, humanito))

    assert(luchadoresLuegoDeUsarItemRoma._1 === mrSatan && luchadoresLuegoDeUsarItemRoma._2 === humanito)
  }

  "androide18" should "queda igual porque los androides no se modifican al recibir ataque de item Romo" in {

    val luchadoresLuegoDeUsarItemRoma = UsarItem(Roma)((humanoConItemRomo, androide18))

    assert(luchadoresLuegoDeUsarItemRoma._1 === humanoConItemRomo && luchadoresLuegoDeUsarItemRoma._2 === androide18)
  }

  "bulma" should "quedar iconsciente al recibir ataque con item Romo y tener menos de 300 de ki" in {

    val luchadoresLuegoDeUsarItemRoma = UsarItem(Roma)((humanoConItemRomo, bulma))

    assert(luchadoresLuegoDeUsarItemRoma._2.estado === Inconsciente)
  }

  "bulma" should "quedar nomal al recibir ataque con item Romo de alguien que no posee dicho item aunque tiene menos de 300 de ki" in {

    val humanoSinItemRomo = Guerrero(Humano, 1000, 50000, List(), Set(), Normal)
    val luchadoresLuegoDeUsarItemRoma = UsarItem(Roma)((humanoSinItemRomo, bulma))

    assert(luchadoresLuegoDeUsarItemRoma._2.estado === Normal)
  }

  //Test usarItem Filoso
  "yajirobe" should "disminuir en 10 (1000/100) el ki de mrSatan al atacarlo con arma filosa" in {

    val luchadoresLuegoDeUsarItemFiloso = UsarItem(Filosa)((yajirobe, mrSatan))

    assert(luchadoresLuegoDeUsarItemFiloso._2.ki === 990)
  }

  "gokuNormal" should " pierde cola" in {
    assertResult(Saiyajin(false, 1, false)) {
      UsarItem(Filosa)((yajirobe, gokuNormal))._2.raza
    }
  }

  "monoGigante" should " pierde cola y se queda saiyajin petoso" in {
    assertResult(Saiyajin(false, 1, false)) {
      UsarItem(Filosa)((yajirobe, monoGigante))._2.raza
    }
  }
  "yajirobe" should "cortar cola de goku y quedar en 1 de ki por ser este un saiyajin con cola y darle con arma filosa" in {

    val luchadoresLuegoDeUsarItemFiloso = UsarItem(Filosa)((yajirobe, gokuSS3))

    assert(luchadoresLuegoDeUsarItemFiloso._2.raza === Saiyajin(false, 3) && luchadoresLuegoDeUsarItemFiloso._2.ki === 1)
  }

  //Test usarItem Fuego
  "rambo" should "no hacer nada a ninguna raza porque no tiene munición en su arma de fuego" in {

    val ramboSinBalas = Guerrero(Humano, 2000, 3000, List(Fuego(0)), Set(), Normal)

    val luchadoresLuegoDeUsarItemFuego = UsarItem(Fuego(0))((ramboSinBalas, humanoConItemRomo))

    assert(luchadoresLuegoDeUsarItemFuego._2.ki === 1000)
  }

  "rambo" should "hacer 20 de danio raza Humana y disminuir en 1 su municion en el arma" in {

    val luchadoresLuegoDeUsarItemFuego = UsarItem(Fuego(100))((ramboConMunicion, humanoConItemRomo))

    val armaFuegoDisminuyoMunicion = luchadoresLuegoDeUsarItemFuego._1.items.find { item => item === Fuego(99) }

    assert(luchadoresLuegoDeUsarItemFuego._2.ki === 980 && armaFuegoDisminuyoMunicion.isEmpty === false)
  }

  "rambo" should "hacer 10 de danio raza Namekusein inconsciente y disminuir en 1 su municion en el arma" in {

    val dendeInconsciente = Guerrero(Namekusein, 100, 200, List(), Set(), Inconsciente)
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

    val luchadoresLuegoDeUsarItemSemilla = UsarItem(SemillaDelErmitaño)((karin, gokuSS3))

    assert(luchadoresLuegoDeUsarItemSemilla._1.ki === luchadoresLuegoDeUsarItemSemilla._1.kiMax)
  }

  "inconsciente" should "recuperarse a normal" in {
    assertResult(Normal) {
      UsarItem(SemillaDelErmitaño)((inconsciente, gokuSS3))._1.estado
    }
  }

  //Test Magia
  "dende" should "aplicar una magia dada, ya que es Namekusein" in {

    val luchadoresLuegoDeMagia = Magia(magiaDende)(dende, mrSatan)

    assert(luchadoresLuegoDeMagia._1.ki === 200 && luchadoresLuegoDeMagia._2.ki === 800)

  }

  "mostruoFeo" should "aplicar una magia dada, ya que es Monstruo" in {

    val luchadoresLuegoDeMagia = Magia(magiaLoca)(monstruoFeo, karin)

    val tieneItemSemillaDelErmitaño = luchadoresLuegoDeMagia._2.items.find { item => item === SemillaDelErmitaño }

    assert(luchadoresLuegoDeMagia._1.estado === Inconsciente && tieneItemSemillaDelErmitaño.isEmpty === true)
  }

  "krilin" should "aplicar magia dado que tiene las 7 esferas!" in {

    val luchadoresLuegoDeMagia = Magia(superMagia)(krilin, gokuSS3)

    assert(luchadoresLuegoDeMagia._1.ki === 10000 && luchadoresLuegoDeMagia._2.ki === 0)
  }

  "mrSatan" should "no hacer magia, ya que es un simple humano sin esferas en su haber" in {

    val luchadoresLuegoDeMagia = Magia(superMagia)(mrSatan, gokuSS3)

    assert(luchadoresLuegoDeMagia._1.ki === 1000 && luchadoresLuegoDeMagia._2.ki === 20000)
  }

}