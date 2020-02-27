

object FunctionalDataStructures{
	case class Queue[A](front: List[A], back: List[A])

	def enqueue[A](elt: A, q: Queue[A]): Queue[A] = q match {
		case Queue(Nil, Nil) => Queue(Nil, List(elt))
		case Queue(front, back) => Queue(front, List(elt) ::: back)
	}
	

	def dequeue[A](q: Queue[A]): Option[(A, Queue[A])] = q match {
		case Queue(Nil, Nil) => None
		case Queue(Nil, back) => Some((back.reverse.head, Queue(back.reverse.tail, Nil)))
		case Queue(front, Nil) => Some((front.head, Queue(front.tail, Nil)))
		case Queue(front, back) => Some((front.head, Queue(front.tail, back)))
	}
	

	sealed trait JoinList[A] { 
		val size: Int
	}

	case class Empty[A]() extends JoinList[A] { 
		val size = 0
	}

	case class Singleton[A](elt: A) extends JoinList[A] { 
		val size = 1
	}

	case class Join[A](lst1: JoinList[A], lst2: JoinList[A], size: Int) extends JoinList[A] 

	def max[A](lst: JoinList[A], compare: (A, A) => Boolean): Option[A] = lst match {
		case Empty() => None
		case Singleton(m) => Some(m)
		case Join(front, back, k) => (max(front, compare), max(back, compare)) match {
			case (None, None) => None
			case (None, Some(y)) => Some(y)
			case (Some(x), None) => Some(x)
			case (Some(x), Some(y)) => {
				if(compare(x, y)){
					Some(x)
				}else{
					Some(y)
				}	
			}
		}
	}

	def first[A](lst: JoinList[A]): Option[A] = lst match {
		case Empty() => None
		case Singleton(m) => Some(m)
		case Join(Singleton(m), Empty(), k) => Some(m)
		case Join(m, n, k) => {
			if(m == Empty()){
				first(n)
			}else{
				first(m)
			}
		}
	}
	

	def rest[A](lst: JoinList[A]): Option[JoinList[A]] = lst match {
		case Empty() => None
		case Singleton(m) => Some(Empty())
		case Join(m, n, k) => rest(m) match {
			case None => rest(n)
			case Some(Empty()) => Some(n)
			case Some(x) => Some(Join(x, n, x.size+n.size))
		}
	}
	
	def nth[A](lst: JoinList[A], n: Int): Option[A] = lst match {
		case Empty() => None
		case Singleton(m) => {
			if(n == 0){
				Some(m)
			}else{
				None
			}	
		}
		case Join(a, b, k) => {
			if(n < 0 || n >= k){
				None
			}
			else if(n < a.size){
				nth(a, n)
			}
			else{
				nth(b, n-a.size)
			}
		}
	}
	

	def map[A,B](f: A => B, lst: JoinList[A]): JoinList[B] = lst match{
		case Empty() => Empty()
		case Singleton(m) => Singleton(f(m))
		case Join(m, n, k) => {
			val fst = map(f, m)
			val snd = map(f, n)
			Join(fst, snd, fst.size+snd.size)
		}
	}

	def filter[A](pred: A => Boolean, lst: JoinList[A]): JoinList[A] = lst match {
		case Empty() => Empty()
		case Singleton(m) => {
			if(pred(m)){
				Singleton(m)
			}else{
				Empty()
			}
		}
		case Join(m, n, k) => {
			val fst = filter(pred, m)
			val snd = filter(pred, n)
			Join(fst, snd, fst.size+snd.size)
		}
	}
}