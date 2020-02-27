import Lists ._
class TestSuite extends org . scalatest . FunSuite {
test (" oddNumbers properly defined ") {
assert ( oddNumbers == List (1 , 3 , 5))
}

test ("sumDouble is correct") {
	assert(sumDouble(List()) == 0)
	assert(sumDouble(List(1, 2, 3)) == 12)
}

test ("removeZeroes is correct") {
	assert(removeZeroes(List()) == Nil)
	assert(removeZeroes(List(0, 2, 1)) == List(2, 1))
	assert(removeZeroes(List(0, 3, 4, 0, 2)) == List(3, 4, 2))
}

test ("countEvens is correct") {
	assert(countEvens(List()) == 0)
	assert(countEvens(List(1, 2, 3, 4, 5)) == 2)
}

test ("removeAlteranting is correct") {
	assert(removeAlternating(List("A", "B")) == List("A")) 
	assert(removeAlternating(List("A", "B")) != List("B"))
}

test ("isAscending is correct") {
	assert(isAscending(List()) == true)
	assert(isAscending(List(4, 3, 1, 5, 10)) == false)
	assert(isAscending(List(1, 1, 2, 2, 2, 3, 5, 5, 3, 4)) == false)
	assert(isAscending(List(1, 1, 2, 2, 3, 3)) == true)
	assert(isAscending(List(1, 2, 3, 4, 5)) == true)
}

test ("addSub is correct") {
	assert(addSub(List(1, 2, 3, 4, 5)) == 3)
	assert(addSub(List()) == 0)
}

test ("alternate is correct") {
	assert(alternate(List(1, 3, 5), List(2, 4, 6)) == List(1, 2, 3, 4, 5, 6))
	assert(alternate(List(), List()) == List())
	assert(alternate(List(), List(1, 2, 3)) == List(1, 2, 3))
	assert(alternate(List(1, 2, 3), List()) == List(1, 2, 3))
	assert(alternate(List(1, 2, 3), List(3, 2, 1)) == List(1, 3, 2, 2, 3, 1))
}

test ("fromTo is correct") {
	assert(fromTo(9, 13) == List(9, 10, 11, 12))
	assert(fromTo(10, 2) == List())
}

test ("insertOrder is correct") {
	assert(insertOrdered(0, List()) == List(0))
	assert(insertOrdered(5, List(1, 3, 7, 9)) == List(1, 3, 5, 7, 9))
}

test ("sort is correct") {
	assert(sort(List()) == Nil)
	assert(sort(List(1, 2, 3, 4, 5)) == List(1, 2, 3, 4, 5))
	assert(sort(List(4, 2, 5, 3, 1)) == List(1, 2, 3, 4, 5))
	assert(sort(List(2, 2, 2, 1, 3, 3, 4)) == List(1, 2, 2, 2, 3, 3, 4))
}
}