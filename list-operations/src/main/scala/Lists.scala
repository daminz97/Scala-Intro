object Lists {
val oddNumbers = 1 :: 3 :: 5 :: Nil 

def sumDouble(ilist: List[Int]): Int = ilist match{
	case Nil => 0
	case m :: tail => m * 2 + sumDouble(tail)
}

def removeZeroes(ilist: List[Int]): List[Int] = ilist match{
	case Nil => Nil
	case m :: tail => {
		if(m != 0){
			m :: removeZeroes(tail)
		}
		else{
			removeZeroes(tail)
		}
	}
}

def countEvens(ilist: List[Int]): Int = ilist match {
	case Nil => 0
	case m :: tail => {
        if(m % 2 == 0){
          1 + countEvens(tail)}
        else{
          countEvens(tail)}
    }
}

def removeAlternating(ilist: List[String]): List[String] = ilist match {
	case Nil => Nil
	case m :: Nil => List(m)
	case m :: n :: Nil => List(m)
	case m :: n :: tail => m :: removeAlternating(tail)
}

def isAscending(ilist: List[Int]): Boolean = ilist match {
	case Nil => true
	case m :: Nil => true
	case m :: n :: tail => {
		if(m <= n)
		  isAscending(n :: tail)
		else
		  false
		
	} 
}


def addSub(ilist: List[Int]): Int = ilist match {
	case Nil => 0
	case m :: Nil => m
	case m :: n :: Nil => m - n
	case m :: n :: tail => m - n + addSub(tail)
}

def alternate(list1: List[Int], list2: List[Int]): List[Int] = (list1, list2) match {
	case (Nil, Nil) => Nil
	case (m :: Nil, Nil) => m :: Nil
	case (Nil, a :: Nil) => a :: Nil
	case (m :: Nil, a :: Nil) => m :: a :: Nil
	case (m :: tail, Nil) => m :: alternate(tail, Nil)
	case (Nil, a :: tail) => a :: alternate(Nil, tail)
	case (m :: tail1, a :: tail2) => m :: a :: alternate(tail1, tail2)
}

def fromTo(numa: Int, numb: Int): List[Int] = {
	if(numa < numb){
		numa :: fromTo(numa+1, numb)
	}
	else{
		Nil
	}
}

def insertOrdered(n: Int, st: List[Int]): List[Int] = st match {
	case Nil => n :: Nil
	case head :: tail => {
		if(n < head){
			n :: st
		}
		else{
			head :: insertOrdered(n, tail)
		}
	}
}

def sort(st: List[Int]): List[Int] = st match {
	case Nil => Nil
	case m :: Nil => m :: Nil
	case m :: n :: Nil => {
		if(m <= n){
			m :: n :: Nil		
		}
		else{
			n :: m :: Nil
		}	
	}
	case m :: n :: tail => {
		if(m <= n){
			insertOrdered(n, sort(m :: tail))
		}
		else{
			insertOrdered(m, sort(n :: tail))
		}
		
	}
}

}