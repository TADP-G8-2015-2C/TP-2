package simuladorTest

object worksheet1 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
	//val a: scala.collection.immutable.Set[Int] = Set(1, 3, 2)
	val a = Set(1, 3) + 2 + 3                 //> a  : scala.collection.immutable.Set[Int] = Set(1, 3, 2)
	a + 4                                     //> res0: scala.collection.immutable.Set[Int] = Set(1, 3, 2, 4)
	a+2                                       //> res1: scala.collection.immutable.Set[Int] = Set(1, 3, 2)
//  a.+:(4)

	a + 3                                     //> res2: scala.collection.immutable.Set[Int] = Set(1, 3, 2)
	a ++ Set(1,99)                            //> res3: scala.collection.immutable.Set[Int] = Set(1, 3, 2, 99)
	math.pow(10, 2)                           //> res4: Double = 100.0
	
}