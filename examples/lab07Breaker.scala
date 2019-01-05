object lab07Breaker {
	abstract class Option[A_B]
	case class Some[A](value: A) extends Option[A]
	case class None[A]() extends Option[A]

  abstract class List[manyLettersAndAlso_numbers42]
  case class Cons[B](head: B, tail: List[B]) extends List[B]
  case class Nil[A]() extends List[A]

  abstract class C

  def add(elem: C, l: List[C]): List[C] = {
    Cons[C](elem, l)
  }

  def head[F](list: List[F]): F = {
    list match {
      case Cons(h, _) => h
      case Nil() => error("called head on empty list")
    }
  }

	def get[B](o : Option[B]) : B = {
		o match {
			case Some(a) => a
			case None() => error("The option is empty.")
		}

	}

	val x : List[Option[String]] = Cons[Option[String]](Some[String]("foo"),
    Cons[Option[String]](Some[String]("bar"), Cons[Option[String]](None[String](), Nil[Option[String]]())));

  //val y : List[Int] = Cons(2, Nil());

  val hello : String = "hello";
  val o : Option[String] = Some[String](hello);
  val p : Option[Int] = Some[Int](42);
  val q : Option[Unit] = Some[Unit](());

  Std.printString(get[String](head(x)))
}
