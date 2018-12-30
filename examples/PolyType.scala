object PolyType {

  abstract class List[A]

  case class Cons[B](head: B, tail: List[B]) extends List[B]

  case class Nil[A]() extends List[A]

  def add[C](elem: C, l: List[C]): List[C] = {
    Cons[C](elem, l)
  }

  def printList(l : List[Int]) : Unit = { 
    l match {
    case Cons(el, tail) => Std.printInt(el); printList(tail)
    case Nil() => ()
    }
    }

  val x: List[Int] = Cons[Int](2, Nil[Int]());
  x match {
    case Cons(h, t) => Std.printInt(h)
  };
  printList(Cons[Int](5, Cons[Int](4, Nil[Int]())))
}
