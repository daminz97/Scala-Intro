import hw.generics._


sealed trait BinTree[A] extends ListLike[A, BinTree[A]]
case class Node[A](lhs: BinTree[A], value: A, rhs: BinTree[A]) extends BinTree[A] {
	def cons(head: A): BinTree[A] = {
		new Node(Leaf(), head, this)
	}

	def head(): Option[A] = {
		if(lhs.isEmpty){
			Some(value)
		}else{
			lhs.head
		}
	}

	def isEmpty(): Boolean = {
		false
	}

	def tail(): Option[BinTree[A]] = lhs match {
		case Leaf() => Some(rhs)
		case Node(lhs, value, rhs) => Some(Node(lhs.tail.get, value, rhs))
	}
}
case class Leaf[A]() extends BinTree[A] {
	def cons(head: A): BinTree[A] = {
		new Node(Leaf(), head, Leaf())
	}

	def head(): Option[A] = {
		None
	}

	def isEmpty(): Boolean = {
		true
	}

	def tail(): Option[BinTree[A]] = {
		None
	}
}

case class IntLike(a: Int) extends hw.generics.Ordered[IntLike]{
	def compare(other: IntLike): Ordering = {
		if(this.a > other.a) GT
		else if(this.a < other.a) LT
		else EQ 
	}
} 


object ListFunctions {
	// def filter(f, alist)
	// def append(alist1 , alist2)
	def sort[A <: hw.generics.Ordered[A], C <: hw.generics.ListLike[A, C]](alist: C): C= {
		if(alist.isEmpty){
			alist
		}else{
			insertOrdered[A,C](alist.head.get, sort[A,C](alist.tail.get))
		}
		
	}

	def insertOrdered[A <:Ordered[A], C <: ListLike[A,C]](m: A, alist: C): C = {
		if(alist.isEmpty){
			alist.cons(m)
		}else{
			(m.compare(alist.head.get) == LT) match{
			case true => alist.tail.get.cons(alist.head.get).cons(m)
			case false => insertOrdered[A,C](m,alist.tail.get).cons(alist.head.get)
			}
		}
	}
	

	def filter[A, L <: ListLike[A, L]](f: A => Boolean, alist: L): L = {
		if(alist.isEmpty){
			alist
		}else{
			f(alist.head.get) match {
				case true => filter(f, alist.tail.get).cons(alist.head.get)
				case false => filter(f, alist.tail.get)
			}	
		}	
	}

	def append[A, L <: ListLike[A, L]](alist1: L, alist2: L): L = {
		if(alist1.isEmpty){
			alist2
		}else{
			append[A, L](alist1.tail.get, alist2).cons(alist1.head.get)
		}
	}
}


class C1 extends T2[Int, Int, String, String] with T3[Int,Int,Int,String,String,String,Int]{
	def f(a: Int, b: Int): Int = 0
	def g(c: String): String = " "
	def h(d: String): Int = 0		
}

class C2 extends T1[Int, Int] with T2[Int, Int, Int, Int] with T3[Int, Int, Int, Int, Int, Int, Int]{
	def f(a: Int, b: Int): Int = 0
	def g(c: Int): Int = 0
	def h(d: Int): Int = 0		
}

class C3[A](x: A) extends T3[Int, A, Int, A, String, String, A]{
	def f(a: Int, b: A): Int = 0
	def g(c: A): String = " "
	def h(d: String): A = x		
}

class C4[A](x: Int, y: C4[A]) extends T1[Int, C4[A]] with T3[Int,C4[A],C4[A],Int,C4[A],C4[A],Int]{
	def f(a: Int, b: C4[A]): C4[A] = b
	def g(c: Int): C4[A] = y
	def h(d: C4[A]): Int = x		
}