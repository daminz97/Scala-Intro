
class TestSuite extends org.scalatest.FunSuite { 

	import FunctionalDataStructures._

	def fromList[A](alist: List[A]): JoinList[A] = alist match { 
		case Nil => Empty()
		case List(x) => Singleton(x)
		case _ => {
			val len = alist.length
			val (lhs, rhs) = alist.splitAt(len / 2) 
			Join(fromList(lhs), fromList(rhs), len)
		}
	}

	val emptyQue = Queue(List(0),List())
	val firstque = Queue(List(1,2,3), List(5,4))
	val emptyjoin = Empty()
	val joinlst = Join(Singleton(3),Singleton(5),2)


	def toList[A](alist: JoinList[A]): List[A] = alist match { 
		case Empty() => Nil
		case Singleton(x) => List(x)
		case Join(alist1 , alist2 , _) => toList(alist1) ++ toList(alist2)
	} 

	test("toList correct") {
		assert(toList(Join(Singleton(1), Singleton(4),2)) == List(1,4))
	}

	test("fromList correct") {
		assert(fromList(List(1,4)) == Join(Singleton(1), Singleton(4),2))
	}

	test("enqueue") {
		assert(enqueue(6, firstque) == Queue(List(1,2,3),List(6,5,4)))
		assert(enqueue(1, emptyQue) == Queue(List(0),List(1)))
	}

	test("dequeue correct") {
		assert(dequeue(firstque) == Some((1,Queue(List(2,3),List(5,4)))))
		assert(dequeue(emptyQue) == Some((0,Queue(List(),List()))))
	}

	// test("max correct") {
	// 	def compare(x: A, y: A): Boolean = x > y
	// 	assert(max(emptyjoin,compare) == None)
	// 	assert(max(joinlst,compare) == Some(5))
	// }

	test("first correct") {
		// assert(first(emptyjoin) == None)
		assert(first(joinlst) == Some(3))
	}

	test("rest correct") {
		// assert(rest(emptyjoin) == None)
		assert(rest(joinlst) == Some(Singleton(5)))
	}

	test("nth correct") {
		// assert(nth(emptyjoin,1) == None)
		assert(nth(joinlst,1) == Some(5))
	}

	// test("map correct") {
	// 	def f(x: A): B = x[B]
	// 	assert(map(f,emptyjoin) == Empty())
	// 	assert(map(f,JoinList) == Join(Singleton))
	// }

	// test("filter correct") {
	// 	def pred(x: A): Boolean
	// 	assert(filter(pred, joinlst) == )
	// }
}






