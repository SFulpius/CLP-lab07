object Std {

	def printString(s : String) : Unit = {
   "do nothing" ++ "again"; ()
}

def print() : Unit = {
    "let's do nothing"; ()
}

def printInt(x : Int) : Unit = {
   ()	
}
abstract class List
case class Nil() extends List
case class Cons(h: Int, t: List) extends List

}
