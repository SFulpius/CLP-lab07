object Lists{

    abstract class List[A]
    case class Cons[A](head : A, tail : List[A]) extends List[A]
    case class Nil[A]() extends List[A]

    def head[A](l : List[A]) : A = {
        l match {
            case Cons(e,_) => e    
            case Nil() => error("The list is empty.")
        }
    }
    
    def tail[A](l  : List[A]) : List[A] = {
        l match {
            case Cons(_,tail) => tail
            case Nil() => error("The list is empty.")
        }
    }
    
    def reverse0[A](result : List[A], l : List[A]) : List[A] = {
        l match {
            case Cons(e,t) => reverse0[A](Cons[A](e, result), t)
            case Nil() => result
        }
    }

    def reverse[A](l : List[A]) : List[A] = {
        reverse0[A](Nil[A](), l)
    }

    def printList0(l : List[String]) : String = {
        l match {
            case Cons(el, t) => ", " ++ el ++ printList0(t)
            case Nil() => ""
        }
    }

    def printList(l : List[String]) : Unit = {
        val s : String = l match {
            case Cons(el, tail) => el ++ printList0(tail)
            case _ => ""
        };
        Std.printString("[" ++ s ++ "]")
    }
    
    val l : List[String] = Cons[String]("Amy", Cons[String]("is" , 
    Cons[String]("fun", Nil[String]())));
    Std.printString(head[String](l));
    printList(l);
    printList(reverse[String](l))
}
