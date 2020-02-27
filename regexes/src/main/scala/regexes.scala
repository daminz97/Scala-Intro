import scala.util.matching.Regex


object Regexes extends hw.regex.RegexLike { 
	val str = "[A-Za-z0-9]"
	def notAlphanumeric: Regex = {
		"[^ A-Za-z0-9]".r
	}

	def time: Regex = {
		"[0-2][0-3]:[0-5][0-9]".r
	}

	def phone: Regex = {
		"([0-9]{3}) [0-9]{3} [0-9]{4}".r
	}

	def zip: Regex = {
		"([0-9]{5}) | ([0-9]{5}-[0-9]{4})".r
	}

	def comment: Regex = {
		"/*.**/".r
	}
	def numberPhrase: Regex = ??? 

	def roman: Regex = {
		"X{0,3})(IX|IV|V?I{0,3}".r
	}

	def date: Regex = ???
	def evenParity: Regex = {
		"[02468]*([13579]{2})*[02468]*".r
	}
}
