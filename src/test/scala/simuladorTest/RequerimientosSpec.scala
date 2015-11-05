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
  //goku.movimentoMasEfectivoContra(vegeta)(unCriterio)
  //mayorDaño  oponenteConMasKi  perderMenorCantDeItems noMeMato
  "gokuNormal" should "movimiento mas efectivo contra vegeta con mayor ventaja de ki" in {
    val masEfectivoPosta: Movimiento = goku.movimientos.maxBy { m => mayorVentajaKi(m(goku, vegeta)) }
    assertResult(masEfectivoPosta) {
      goku.movimientoMasEfectivoContra(vegeta)(mayorVentajaKi)
    }
  }

  "gokuNormal" should "movimiento mas efectivo contra vegeta con mayorDaño" in {
    val masEfectivoPosta: Movimiento = goku.movimientos.maxBy { m => mayorDaño(m(goku, vegeta)) }
    assertResult(masEfectivoPosta) {
      goku.movimientoMasEfectivoContra(vegeta)(mayorDaño)
    }
  }
    "gokuNormal" should "movimiento mas efectivo contra vegeta con oponenteConMasKi" in {
    val masEfectivoPosta: Movimiento = goku.movimientos.maxBy { m => oponenteConMasKi(m(goku, vegeta)) }
    assertResult(masEfectivoPosta) {
      goku.movimientoMasEfectivoContra(vegeta)(oponenteConMasKi)
    }
  }

  "gokuNormal" should "movimiento mas efectivo contra vegeta con perderMenorCantDeItems" in {
    val masEfectivoPosta: Movimiento = goku.movimientos.maxBy { m => perderMenorCantDeItems(m(goku, vegeta)) }
    assertResult(masEfectivoPosta) {
      goku.movimientoMasEfectivoContra(vegeta)(perderMenorCantDeItems)
    }
  }
  
  "gokuNormal" should "movimiento mas efectivo contra vegeta con noMeMato" in {
    val masEfectivoPosta: Movimiento = goku.movimientos.maxBy { m => noMeMato(m(goku, vegeta)) }
    assertResult(masEfectivoPosta) {
      goku.movimientoMasEfectivoContra(vegeta)(noMeMato)
    }
  }

}