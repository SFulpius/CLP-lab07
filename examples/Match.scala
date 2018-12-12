object Match {

def add(x : Int, y : Int, b : Boolean) : Int = {if(b) {x+y} else{ error("ERROR")}}
abstract class List
case class Nil() extends List
case class Cons(head : Int, tail : List) extends List

	val x : Int = 1 + 2;
	x match {
	  case 2 => 42
	  case 6 => 5
	  case y => y
      case _ => x
	};
val y : List = Cons(4, Nil());
val z : List = y match {
  //  case Cons(2, Cons(4, a)) => ()
  //  case Nil() => ()
    case a => a
}; 
Nil() match {case Nil() => 
        2 match {
            case a => a + 5
            case _ => 42
            case 42 => 2
        }
        case Cons(_,_) => 3
        case Cons(Nil(),_) => 3
 }

}
