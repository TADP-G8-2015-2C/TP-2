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
    
    
}