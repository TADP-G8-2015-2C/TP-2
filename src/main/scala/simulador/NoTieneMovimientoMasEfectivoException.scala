package simulador

case class NoHayMovMasEfectivoParaGenerarPlanException(smth:String) extends RuntimeException(smth) {
  
}