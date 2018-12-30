object PolyType {

  abstract class List[A]

  case class Cons[B](head: B, tail: List[B]) extends List[B]

  case class Nil[A]() extends List[A]

  def add[C](elem: C, l: List[C]): List[C] = {
    Cons[C](elem, l)
  }

  val x: List[Int] = Cons[Int](2, Nil[Int]());
  val z: String = 42;
  x match {
    case Cons(h, t) => Std.printString(h)
  }

}
