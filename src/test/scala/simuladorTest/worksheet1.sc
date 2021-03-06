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
	a.filter { n => n> 1 }                    //> res5: scala.collection.immutable.Set[Int] = Set(3, 2)
	val li=List.range(1, 5)                   //> li  : List[Int] = List(1, 2, 3, 4)
	li.:+(6)                                  //> res6: List[Int] = List(1, 2, 3, 4, 6)
	true.getClass()                           //> res7: Class[Boolean] = boolean
		
}