object lab07Breaker {
	abstract class Option[A]
	case class Some[A](value: A) extends Option[A]
	case class None[A]() extends Option[A]

	def get[A](o : Option[A]) : A = {
		o match {
			case Some(a) => a
			case None() => error("The option is empty.")
		}
	}

	val x : String = "foo";
	val o : Option[String] = Some[String](x);
	val p : Option[Int] = Some[Int](42);
  val q : Option[Unit] = Some[Unit](());
	Std.printString(get[String](o) ++ "bar")
}
