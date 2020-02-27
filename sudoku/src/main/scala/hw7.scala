import hw.sudoku._

object Solution extends SudokuLike {
	type T = Board

	val lst = List(1,2,3,4,5,6,7,8,9)

	def parse(str: String): Board = {
		new Board(parseHelper(str, 0))
	}
	
	def parseHelper(str: String, pos:Int): Map[(Int, Int), List[Int]] = {
		if(pos == 80){
			if(str.charAt(pos) == '.'){
				Map{((8,8), compare(getValue(getPos(peers(pos/9, pos%9)), str), lst))}
			}else{
				Map{((8,8), List(str.charAt(80).asDigit))}
			}
		}else{
			if(str.charAt(pos) == '.'){
				val update = compare(getValue(getPos(peers(pos/9, pos%9)), str), lst)
				update.size match {
					case 1 => Map{((pos/9, pos%9),update)}++parseHelper(List(pos).foldLeft(str)((coor, value) => coor.updated(value,(update.head+'0').toChar)),pos+1)
					case _ => Map{((pos/9, pos%9),update)}++parseHelper(str,pos+1)
				}
			}else{
				Map{((pos/9,pos%9), List(str.charAt(pos).asDigit))}++parseHelper(str, pos+1)
			}			
		}
	}

	def getPos(lst: List[(Int,Int)]): List[Int] = lst match {
		case Nil => Nil
		case head :: tail => (head._1*9 + head._2) :: getPos(tail)
	}

	def getValue(poslst: List[Int], str: String): List[Int] = poslst match {
		case Nil => Nil
		case head :: tail => {
			if(str.charAt(head) == '.') getValue(tail, str)
			else (str.charAt(head).toInt-48) :: getValue(tail, str)
		}
	}

	def compare(alist: List[Int], blist: List[Int]): List[Int] = blist match {
		case Nil => Nil
		case head :: tail => {
			if(alist.contains(head)){
				compare(alist,tail)
			}else{
				head :: compare(alist,tail)
			}	
		}	
	}


	//You can use a Set instead of a List (or, any Iterable)
	def peers(row: Int, col: Int): List[(Int, Int)] = {
		rowHelper(row,0,row,col,8):::colHelper(0,col,row,col,8):::blockHelper(row,col)
	} 

	def rowHelper(start:Int,tail:Int,x:Int,y:Int,size:Int): List[(Int,Int)] = {
		if(tail>size) Nil
		else{
			if(start==x && tail==y){
				rowHelper(start,tail+1,x,y,size)
			}else{
				(start,tail)::rowHelper(start,tail+1,x,y,size)
			}
		}
	}
	def colHelper(start:Int,tail:Int,x:Int,y:Int,size:Int): List[(Int,Int)] = {
		if(start>size) Nil
		else{
			if(start==x && tail==y){
				colHelper(start+1,tail,x,y,size)
			}else{
				(start,tail)::colHelper(start+1,tail,x,y,size)
			}
		}
	}
	def blockHelper(row:Int,col:Int): List[(Int,Int)] = {
		if(row/3==0){
			if(col/3==0){
				rowHelper(0,0,row,col,2):::rowHelper(1,0,row,col,2):::rowHelper(2,0,row,col,2)
			}else if(col/3==1){
				rowHelper(0,3,row,col,5):::rowHelper(1,3,row,col,5):::rowHelper(2,3,row,col,5)
			}else{
				rowHelper(0,6,row,col,8):::rowHelper(1,6,row,col,8):::rowHelper(2,6,row,col,8)
			}	
		}else if(row/3==1){
			if(col/3==0){
				rowHelper(3,0,row,col,2):::rowHelper(4,0,row,col,2):::rowHelper(5,0,row,col,2)
			}else if(col/3==1){
				rowHelper(3,3,row,col,5):::rowHelper(4,3,row,col,5):::rowHelper(5,3,row,col,5)
			}else{
				rowHelper(3,6,row,col,8):::rowHelper(5,6,row,col,8):::rowHelper(5,6,row,col,8)
			}	
		}else{
			if(col/3==0){
				rowHelper(6,0,row,col,2):::rowHelper(7,0,row,col,2):::rowHelper(8,0,row,col,2)
			}else if(col/3==1){
				rowHelper(6,3,row,col,5):::rowHelper(7,3,row,col,5):::rowHelper(8,3,row,col,5)
			}else{
				rowHelper(6,6,row,col,8):::rowHelper(7,6,row,col,8):::rowHelper(8,6,row,col,8)
			}	
		}	
	}
}
	// Top-left corner is (0,0). Bottom-right corner is (8,8). Feel free to
	// change the fields of this class.
	



	class Board(val available: Map[(Int, Int), List[Int]]) extends BoardLike[Board] {
	

	def availableValuesAt(row: Int, col: Int): List[Int] = {
		// Assumes that a missing value means all values are available. Feel // free to change this.
		available.getOrElse((row, col), 1.to(9).toList)
	}
	

	def valueAt(row: Int, col: Int): Option[Int] = {
		if(availableValuesAt(row,col).length != 1) None
		else Some(availableValuesAt(row,col).head)
		
	}

	def isSolved (): Boolean = {
		available.forall(m => m._2.size == 1)
	}
	

	def isUnsolvable (): Boolean = {
		available.exists(m => m._2.size == 0)
	}
	

	def place(row: Int, col: Int, value: Int): Board = { 
		require(availableValuesAt(row, col).contains(value))
		new Board(placeHelper((available+((row,col) -> List(value))), Solution.peers(row, col), value))
	}
	// You can return any Iterable (e.g., Stream)
	
	def placeHelper(newMap: Map[(Int, Int), List[Int]], peers: List[(Int, Int)], value: Int): Map[(Int, Int), List[Int]] = peers.size match{
		case 1 => {
			if(availableValuesAt(peers.head._1, peers.head._2).contains(value)){
				newMap.updated(peers.head, newMap(peers.head).diff(List(value)))
			}
			else newMap
		}
		case _ => {
			if(availableValuesAt(peers.head._1, peers.head._2).contains(value)){
				placeHelper(newMap.updated(peers.head, newMap(peers.head).diff(List(value))), peers.tail, value)
			}
			else placeHelper(newMap, peers.tail, value)
		}
	}

	def nextStates(): List[Board] = {
  		???
	}


	
	def solve(): Option[Board] = isSolved match {
		case true => Some(this)
		case _ => solveHelper(this.nextStates)	
	} 

	def solveHelper(boards: List[Board]): Option[Board] = boards match {
		case head :: tail => {
			if(head.isSolved) Some(head)
			else solveHelper(tail)
		}
		case _ => None
	}
}