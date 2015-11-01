
import simulador.ArenaDeCell._
import org.scalatest.FlatSpec
import org.scalatest.Matchers

class TestMovimientos extends FlatSpec with Matchers {
  
  val mrSatan = Guerrero(Humano(), 1000, List(), List(), Normal)
  val bulma = Guerrero(Humano(), 100, List(), List(), Normal)
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
  
  "mrSatan" should "no hacer nada  cuando useItem Romo ya que no lo tiene en su inventario" in {
    
    val humanito = Guerrero(Humano(), 1, List(), List(), Normal)
    val luchadoresLuegoDeUsarItemRoma = usarItem(Roma()) ((mrSatan, Option(humanito)))
    
    assert(luchadoresLuegoDeUsarItemRoma._1 === mrSatan && luchadoresLuegoDeUsarItemRoma._2 === Option(humanito))
  }
  
  "androide18" should "queda igual porque los androides no se modifican al recibir ataque de item Romo" in  {
    
    val luchadoresLuegoDeUsarItemRoma = usarItem(Roma()) ((mrSatan, Option(androide18)))
    
    assert(luchadoresLuegoDeUsarItemRoma._1 === mrSatan && luchadoresLuegoDeUsarItemRoma._2 === Option(androide18))
  }
  
  "bulma" should "quedar iconsciente al recibir ataque con item Romo y tener menos de 300 de ki" in {
    
    val humanoConItemRomo = Guerrero(Humano(), 1000, List(Roma()), List(), Normal)
    val luchadoresLuegoDeUsarItemRoma = usarItem(Roma()) ((humanoConItemRomo, Option(bulma)))
    
    assert(luchadoresLuegoDeUsarItemRoma._2.get.estado === Inconsciente)
  }
  
   "bulma" should "quedar nomal al recibir ataque con item Romo de alguien que no posee dicho item aunque tiene menos de 300 de ki" in {
    
    val humanoSinItemRomo = Guerrero(Humano(), 1000, List(), List(), Normal)
    val luchadoresLuegoDeUsarItemRoma = usarItem(Roma()) ((humanoSinItemRomo, Option(bulma)))
    
    assert(luchadoresLuegoDeUsarItemRoma._2.get.estado === Normal)
  }
}