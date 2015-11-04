package simulador

object TuplasUtils {
  implicit class TuplasUtils2[A, B](tupla: Tuple2[A, B]) {
   
    def onFst[C](funcion: A => C) = {
      tupla.onZipF((funcion,identity(_:B)))//(funcion(tupla._1), tupla._2)
    }
    
    def onSec[C](funcion: B => C) = {
      tupla.onZipF((identity(_:A),funcion))//(tupla._1, funcion(tupla._2))
    }
    
    def onZipF[C,D](funcionTupla: (A => C, B => D)) = {
      (funcionTupla._1(tupla._1),funcionTupla._2(tupla._2))
    }
 
  }
 
  implicit class TuplasUtils1[A](tupla: Tuple2[A, A]) {
      
    def onBoth[C](funcion: A => C) = {
      (funcion(tupla._1), funcion(tupla._2))
    }
  }
}