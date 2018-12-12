object TestInt{

def printInt(x : Int) : Unit = {
		x + 2;
		42
	}
def mul(x: Int, y : Int) : Int = {
    x*y
    }
def square(x: Int) : Int ={
    val x : Int = x*x;
    x
    }

abstract class List
case class Nil() extends List
case class Cons(h: Int, t: List) extends List

//case class mul() extends List

	(val i : Int = 12341; i);
	val x : Int = 2 + 5;
    val i : String = "hello";
	printInt(i);
    mul(i, x);
    square(x);
    val z : List = Nil();
    x match {
    case 2 => 2
    case Std.Nil() => 42
    case Std.Cons(a, Std.Nil()) => 24
    case Nil() => "result"
    case y => 1
    case y => 1
    case Nil => "NILLLL"
    case Cons => "CONS"
    }
	
}
