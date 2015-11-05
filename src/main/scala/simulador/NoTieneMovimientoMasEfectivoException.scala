package simulador

case class NoTieneMovimientoMasEfectivoException(smth:String) extends RuntimeException(smth) {
  
}