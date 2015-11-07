package simuladorTest

import simulador._
import simulador.ArenaDeCell._
import simulador.Guerrero._

import org.scalatest._
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scala.collection.immutable.List
import scala.collection.immutable.Set
import scala.collection.GenTraversableOnce

import simulador.TuplasUtils._
import simuladorTest.SetUp._

class RequerimientosSpec extends FlatSpec with Matchers {

  "goku" should "movimiento mas efectivo contra vegeta con mayor ventaja de ki" in {
    val masEfectivoPosta: Movimiento = goku.movimientos.maxBy { m => mayorVentajaKi(m(goku, vegeta)) }
    assertResult(masEfectivoPosta) {
      goku.movimientoMasEfectivoContra(vegeta)(mayorVentajaKi)
    }
  }

  "goku" should "movimiento mas efectivo contra vegeta con mayorDaño" in {
    val masEfectivoPosta: Movimiento = goku.movimientos.maxBy { m => mayorDaño(m(goku, vegeta)) }
    assertResult(masEfectivoPosta) {
      goku.movimientoMasEfectivoContra(vegeta)(mayorDaño)
    }
  }
  "goku" should "movimiento mas efectivo contra vegeta con oponenteConMasKi" in {
    val masEfectivoPosta: Movimiento = goku.movimientos.maxBy { m => oponenteConMasKi(m(goku, vegeta)) }
    assertResult(masEfectivoPosta) {
      goku.movimientoMasEfectivoContra(vegeta)(oponenteConMasKi)
    }
  }

  "goku" should "movimiento mas efectivo contra vegeta con perderMenorCantDeItems" in {
    val masEfectivoPosta: Movimiento = goku.movimientos.maxBy { m => perderMenorCantDeItems(m(goku, vegeta)) }
    assertResult(masEfectivoPosta) {
      goku.movimientoMasEfectivoContra(vegeta)(perderMenorCantDeItems)
    }
  }

  "goku" should "movimiento mas efectivo contra vegeta con noMeMato" in {
    val masEfectivoPosta: Movimiento = goku.movimientos.maxBy { m => noMeMato(m(goku, vegeta)) }
    assertResult(masEfectivoPosta) {
      goku.movimientoMasEfectivoContra(vegeta)(noMeMato)
    }
  }
  "kami" should "sin movimientos lanza excepcion al pedirle el mas efectivo" in {
    val thrown = intercept[NoTieneMovimientosException] {
      kami.movimientoMasEfectivoContra(goku)(mayorVentajaKi)
    }
    assert(thrown.getMessage === "Antes que aprenda algun movimiento")
  }

  "chiChi" should " lanza excepcion al no tener movimiento mas efectivo con ese criterio" in {
    val thrown = intercept[NoTieneMovimientoMasEfectivoException] {
      chiChi.movimientoMasEfectivoContra(goku)(mayorVentajaKi)
    }
    assert(thrown.getMessage === "No tiene movimiento mas efectivo contra oponente")
  }

  //Test del pelearUnRound
  "piccolo" should "atacar con movimiento que posee, y recibir el mejor ataque posible por defecto de parte del enemigo" in {

    val luchadoresDespuesDeRound = piccolo.pelearUnRound(kamehameha)(freezer)()

    //Explicación del test para entenderlo al hacerlo. Si quieren después lo borro
    //freezer tiene kamehameha (onda(500)), onda(100). 
    //Debería usar el kamehameha porque lo deja mejor en ki con respecto a rival(condicion por defecto)
    //Secuencia del ataque: piccolo le saca (500 / 2) con su onda(500) (al atacar a monstruo divide por 2) 
    //y por eso pierde 500, freezer contrataca con lo mismo (kamehameha) y le saca 1000 a piccolo, perdiendo 500. 
    //Por esto, piccolo termina muerto (empezó con 1500) y freezer empezó con 2000 y termina con 1250.

    assert(luchadoresDespuesDeRound._1.ki === 0 && luchadoresDespuesDeRound._1.estado === Muerto)
    assert(luchadoresDespuesDeRound._2.ki === 1250)
  }
  "freezer" should "ataca con movimiento y enemigo muere, entonces el enemigo con contraataca" in {
    val luchadoresDespuesDeRound = freezer.pelearUnRound(kamehameha)(rojelioCargaKi)()

    //Freezer hace kamehameha y deja a rojelio en 1 de ki (tiene 1001). Rojelio (un Namekusein) tiene entre su movimientos a
    //Magia(superMagia) la cual recupera todo su ki hasta el máximo y disminuye 100000 de ki al oponente
    //matando así a freezer.

    assert(luchadoresDespuesDeRound._2.ki === 1500)
    assert(luchadoresDespuesDeRound._1.ki === 0 && luchadoresDespuesDeRound._1.estado === Muerto)

  }
  //test de plan de ataque
 
  "yamcha" should "planificar ataque contra cell jr" in {
    assertResult(List(UsarItem(Filosa),UsarItem(SemillaDelErmitaño))) {
      yamcha.planDeAtaqueContra(cellJr, 2)(gastaMenosKi)
    }
  }
  
  //test pelearContra
  
  "freezer" should "ganar la pelea contra maestroRoshi al lanzarle un ataque mortal en la primera" in {
    
    val resultadoPelea = freezer.pelearContra(maestroRoshi)(List(onda(500)))
    
    //En este caso, freezer lo ataca con onda(500) y lo mata, por ende no lo contraataca
    //por usar onda, freezer pierde 500. Como era solo un movimiento en el plan,
    //cumplió de llegar al final luego de este primerRound, con freezer como ganador
    
    val freezerLuegoDePelear = freezer.copy(ki = freezer.ki - 500)
    
    assert(resultadoPelea === Ganador(freezerLuegoDePelear))
  }
  
  "freezer" should "ganar la pelea contra maestroRoshi en 1er round, aunque están planificados más ataques" in {
    
    val resultadoPelea = freezer.pelearContra(maestroRoshi)(List(onda(500), kamehameha))
   
    val freezerLuegoDePelear = freezer.copy(ki = freezer.ki - 500)
    
    assert(resultadoPelea === Ganador(freezerLuegoDePelear))
  }
  
  "maestroRoshi" should "pierde la pelea contra freezer, ya que ataca y no mata, y freezer después lo liquida" in {
    
    val resultadoPelea = maestroRoshi.pelearContra(freezer)(List(onda(100)))

    val freezerLuegoDePelear = freezer.copy(ki = freezer.ki - 500 - 50)
    
    assert(resultadoPelea === Ganador(freezerLuegoDePelear))
  }
  
  "piccolo" should "ganar por poco, luego de 2 ataques planificados" in {
    
   val resultadoPelea = piccolo.pelearContra(humano1701KiKamehameha)(List(kamehameha, onda(160)))
   //el kamehameha le saca 1000 al humano, dejandolo en 701. piccolo pierde 500 y queda en 1000
   //a su vez, como humano tiene onda(400), con lo que él queda en 301 y piccolo en 200
   //casi muerto, piccolo tira su onda(160), quedando él en 40 y su rival con 301 - 400 < 0 => Muerto!
       
   val piccoloLuegoDePelear = piccolo.copy(ki = piccolo.ki - 500 - 800 - 160)
   
   assert(resultadoPelea === Ganador(piccoloLuegoDePelear))
   
  }
  
  "piccolo y el humano1701KiKamehameha" should "ninguna ganar y seguir peleando luego de usar todos los mov" in {
    
   val resultadoPelea = piccolo.pelearContra(humano1701KiKamehameha)(List(kamehameha))
   //el kamehameha le saca 1000 al humano, dejandolo en 701. piccolo pierde 500 y queda en 1000
   //a su vez, como humano tiene onda(400), con lo que él queda en 301 y piccolo en 200
       
   val piccoloLuegoDePelear = piccolo.copy(ki = piccolo.ki - 500 - 800)
   val humanoLuegoDePelear = humano1701KiKamehameha.copy(ki = humano1701KiKamehameha.ki - 1000 - 400)
   
   assert(resultadoPelea === SiguenPeleando(piccoloLuegoDePelear, humanoLuegoDePelear))
    
  }
  
}