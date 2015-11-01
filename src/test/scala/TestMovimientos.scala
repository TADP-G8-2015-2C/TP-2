
import simulador.ArenaDeCell._
import org.scalatest.FlatSpec
import org.scalatest.Matchers

class TestMovimientos extends FlatSpec with Matchers {
  
  val mrSatan = Guerrero(Humano(), 1000, List(), List(), Normal)
  val goku = Guerrero(Saiyajin(true, 3), 20000, List(), List(), Normal)
  val androide18 = Guerrero(Androide(1000), 0, List(), List(), Normal)
  
  "mrSatan" should "cargarKi y subir 100 de ki por ser Guerrero" in {

    assert(cargarKi(mrSatan, null)._1.ki === 1100)
  }

  "goku" should "cargarKi y subir ki en  150 * nivel de SS por ser Saiyajin" in {

    assert(cargarKi(goku, null)._1.ki === 20450)
  }
 
  "androide18" should "cargarKi y no subir nada (seguir en 0) por ser un Androide" in {

    assert(cargarKi(androide18, null)._1.ki === 0)
  }
  
}