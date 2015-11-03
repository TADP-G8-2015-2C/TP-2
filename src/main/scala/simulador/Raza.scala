package simulador

abstract class Raza {
  def cortarCola = this
  def kiLuegoDeCortarCola(unGuerrero: Guerrero) = unGuerrero.ki
  def aumentarKiMax(unGuerrero: Guerrero) = unGuerrero.kiMax
  def disminuirKiMax(unGuerrero: Guerrero) = unGuerrero.kiMax
  def subirDeNivel(unGuerrero: Guerrero) = this
  def meQuedeInconsciente(unGuerrero: Guerrero) = unGuerrero
}

case class Monstruo() extends Raza {}
case class Humano() extends Raza {}
case class Androide(bateria: Int = 0) extends Raza {}
case class Namekusein() extends Raza {}
case class Fusionado(guerreroOriginal: Guerrero) extends Raza {}
case class Saiyajin(cola: Boolean, nivel: Int = 0, monoGigante: Boolean = false) extends Raza {

  override def subirDeNivel(guerrero: Guerrero) = {
    if (guerrero.ki >= guerrero.kiMax) {
      copy(nivel = nivel + 1)
    } else {
      this
    }
  }

  override def cortarCola = copy(cola = false, monoGigante = false)
  override def kiLuegoDeCortarCola(unGuerrero: Guerrero) = 1 //TODO lo del MonoGigante como tratarlo
  override def aumentarKiMax(unGuerrero: Guerrero) = unGuerrero.kiMax * 3
  override def disminuirKiMax(unGuerrero: Guerrero) = unGuerrero.kiMax / 3

  override def meQuedeInconsciente(unGuerrero: Guerrero) = {
    unGuerrero.dejarDeSerSS()

  }
}

