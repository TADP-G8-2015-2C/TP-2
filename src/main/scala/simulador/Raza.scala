package simulador

abstract class Raza {
  def aumentarKiMax(unGuerrero: Guerrero) = unGuerrero.kiMax
  def disminuirKiMax(unGuerrero: Guerrero) = unGuerrero.kiMax
}

case object Monstruo extends Raza 
case object Humano extends Raza 
case object Androide extends Raza 
case object Namekusein extends Raza 
case class Fusionado(guerreroOriginal: Guerrero) extends Raza 
case class Saiyajin(cola: Boolean) extends Raza {

  override def aumentarKiMax(unGuerrero: Guerrero) = unGuerrero.kiMax * 3
  override def disminuirKiMax(unGuerrero: Guerrero) = unGuerrero.kiMax / 3

//  require(monoGigante == true, nivel == 0)//si es monoGigante no puede ser saiyajin
//  require(cola == false,monoGigante == false)// si no tiene cola no puede ser monoGigante
  //TODO poner estos require pero bien
}

