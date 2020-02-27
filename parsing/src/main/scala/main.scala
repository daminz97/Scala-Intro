import hw.parsing._
import scala.util.parsing.combinator._

object ArithEval extends ArithEvalLike { 
	def eval(e: Expr): Double = e match{
		case Add(x, y) => eval(x) + eval(y)
		case Sub(x, y) => eval(x) - eval(y)
		case Mul(x, y) => eval(x) * eval(y)
		case Div(x, y) => eval(x) / eval(y)
		case Exponent(x, y) => Math.pow(eval(x), eval(y))
		case Num(x) => x 
	}
}

object ArithParser extends ArithParserLike {
// number: PackratParser[Double] is defined in ArithParserLike

	lazy val atom: PackratParser[Expr] = {
		(number)^^{case x => Num(x)} | ("("~expr~")")^^{case _~e~_ => e}
	}
	
	lazy val exponent: PackratParser[Expr] = {
		(exponent~"^"~atom)^^{case e1~"^"~e2 => Exponent(e1,e2)} | atom
	}

	lazy val mul: PackratParser[Expr] = {
		(mul~"*"~exponent)^^{case e1~"*"~e2 => Mul(e1,e2)} | (mul~"/"~exponent)^^{case e1~"/"~e2 => Div(e1,e2)} | exponent
	}

	lazy val add: PackratParser[Expr] = {
		(add~"+"~mul)^^{case e1~"+"~e2 => Add(e1,e2)} | (add~"-"~mul)^^{case e1~"-"~e2 => Sub(e1,e2)} | mul
	}

	lazy val expr: PackratParser[Expr] = add 
}

object ArithPrinter extends ArithPrinterLike { 
	def print(e: Expr): String = e match{
		case Add(x, y) => "(" + print(x) + "+" + print(y) + ")"
		case Sub(x, y) => "(" + print(x) + "-" + print(y) + ")"
		case Mul(x, y) => "(" + print(x) + "*" + print(y) + ")"
		case Div(x, y) => "(" + print(x) + "/" + print(y) + ")"
		case Exponent(x, y) => "(" + print(x) + "^" + print(y) + ")"
		case Num(x) => x.toString
	}
}