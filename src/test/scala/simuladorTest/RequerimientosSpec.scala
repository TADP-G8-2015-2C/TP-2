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
}