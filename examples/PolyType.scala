object PolyType {
    abstract class List[A]
    case class Cons[A](head : A, tail : List[A]) extends List[A]
    case class Nil[A]() extends List[A]
    def add[A](elem : A, l : List[A]) : List[A] = {
        Cons[A](elem, l)
    }
val x : List[Int] = Cons[Int]( 2, Nil[Int]());
x match {
    case Cons(h,t) => Std.printString(h)
}
}
