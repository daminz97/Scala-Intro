import hw.tictactoe._



class Game(turn: Player, dim: Int, board: Map[(Int, Int), Player]) extends GameLike[Game] { 

	val xdim = this.dim - 1
	val ydim = this.dim - 1

	def isFinished (): Boolean = {
		isWinner(X,board) || isWinner(O,board) || isFull
	}

	
	/* Assume that isFinished is true */
	def getWinner(): Option[Player] = {
		if(isWinner(X, board)) Some(X)
		else if(isWinner(O, board)) Some(O)
		else None
	}


	def isWinner(turn: Player, board: Map[(Int, Int), Player]): Boolean = {
		checkH(ydim, turn, board) || checkV(xdim, turn, board) || checkD(0, turn, board) || checkD(this.xdim, turn, board)
	}

	def checkH(y: Int, turn: Player, board: Map[(Int, Int), Player]): Boolean = y match {
		case 0 => (Hhelper(0, turn, board).size == dim)
		case _ => {
			if(Hhelper(y, turn, board).size == dim){
				true
			}else{
				checkH(y-1, turn, board)
			}	
		}
	}

	def Hhelper(y: Int, turn: Player, board: Map[(Int, Int), Player]): Map[(Int, Int), Player] = {
		board.filterKeys(m => m._2 == y).filter({ case (a,b) => b == turn})
	}

	def checkV(x: Int, turn: Player, board: Map[(Int, Int), Player]): Boolean = x match {
		case 0 => (Vhelper(0, turn, board).size == dim)
		case _ => {
			if(Vhelper(x, turn, board).size == dim){
				true
			}else{
				checkV(x-1, turn, board)
			}	
		}
	}

	def Vhelper(x: Int, turn: Player, board: Map[(Int, Int), Player]): Map[(Int, Int), Player] = {
		board.filterKeys(m => m._1 == x).filter({ case (a,b) => b == turn})
	}

	def checkD(x: Int, turn: Player, board: Map[(Int, Int), Player]): Boolean = x match {
		case 0 => (D1helper(turn, board).size == dim)
		case this.xdim => (D2helper(turn, board).size == dim)
	}

	def D1helper(turn: Player, board: Map[(Int, Int), Player]): Map[(Int, Int), Player] = {
		board.filterKeys(m => m._1 == m._2).filter({ case (a,b) => b == turn})
	}

	def D2helper(turn: Player, board: Map[(Int, Int), Player]): Map[(Int, Int), Player] = {
		board.filterKeys(m => m._1 + m._2 == this.ydim).filter({ case (a,b) => b == turn})
	}

	def isFull(): Boolean = {
		val Xsize = board.filter({case (a,b) => b == X}).size
		val Osize = board.filter({case (a,b) => b == O}).size
		if((Xsize + Osize) == dim*dim){
			true
		}else{
			false
		}
		
	}


	def nextBoards(): List[Game] = {
		if(isFinished){
			Nil
		}else{
			newMap(0, 0, turn)
		}
		
	}

	def newMap(xcoor: Int, ycoor: Int, turn: Player): List[Game] = (xcoor, ycoor) match {
		case (this.xdim, this.ydim) => {
			if(!board.contains(this.xdim, this.ydim)){
				if(turn == X){
					Solution.createGame(O, dim, this.board++Map((this.xdim, this.ydim) -> X)) :: Nil
				}else{
					Solution.createGame(X, dim, this.board++Map((this.xdim, this.ydim) -> O)) :: Nil
				}
			}else{
				Nil
			}	
		}
		case (xcoor, 0) => {
			if(!board.contains(xcoor, 0)){
				if(turn == X){
					Solution.createGame(O, dim, this.board++Map((xcoor, 0) -> X)) :: newMap(xcoor, 1, X)							
				}else{
					Solution.createGame(X, dim, this.board++Map((xcoor, 0) -> O)) :: newMap(xcoor, 1, O)
				}						
			}else{
				newMap(xcoor, 1, turn)
			}	
		}

		case (xcoor, ycoor) => {
			if(!board.contains(xcoor, ycoor)){
				if(ycoor == ydim){
					if(turn == X){
						Solution.createGame(O, dim, this.board++Map((xcoor, ycoor) -> X)) :: newMap(xcoor+1, 0, X)		
					}else{
						Solution.createGame(X, dim, this.board++Map((xcoor, ycoor) -> O)) :: newMap(xcoor+1, 0, O)
					}
				}else{
					if(turn == X){
						Solution.createGame(O, dim, this.board++Map((xcoor, ycoor) -> X)) :: newMap(xcoor, ycoor+1, X)	
					}else{
						Solution.createGame(X, dim, this.board++Map((xcoor, ycoor) -> O)) :: newMap(xcoor, ycoor+1, O)	
					}	
				}
			}else if(ycoor == ydim){
				newMap(xcoor+1, 0, turn)
			}else{
				newMap(xcoor, ycoor+1, turn)
			}
		}
	}
}


object Solution extends MinimaxLike {
	type T = Game // T is an "abstract type member" of MinimaxLike


	def createGame(turn: Player, dim: Int, board: Map[(Int, Int), Player]): Game = {
		new Game(turn, dim, board)
	} 


	def minimax(board: Game): Option[Player] = board.getWinner() match {
		case Some(X) => Some(X)
		case Some(O) => Some(O)
		case None => {
			if(board.isFull){
				None
			}else{
				helper(board.nextBoards)
			}	
		}	
	}
	
	def helper(board: List[Game]): Option[Player] = board match {
		case Nil => None
		case head :: tail => head.getWinner() match {
			case Some(X) => Some(X)
			case Some(O) => Some(O)
			case None => {
				if(head.isFull){
					helper(tail)
				}else if(head.nextBoards == None){
					helper(tail)
				}else{
					helper(head.nextBoards ::: tail)
				}	
			}
		}
	}
}