object Pairs {

  abstract class TwoTypesTuple[A, B]

  case class Pair[A, B](e1: A, e2: B) extends TwoTypesTuple[A, B]

  case class PairHom[A, B](e1: A, e2: A) extends TwoTypesTuple[A, B]

  case class DoublePair[A, B](e11: A, e12: A, e21: B, e22: B) extends TwoTypesTuple[A, B]

  def first[A, B](tuple: TwoTypesTuple[A, B]): A = {
    tuple match {
      case Pair(e, _) => e
      case PairHom(e, _) => e
      case DoublePair(e, _, _, _) => e
    }
  }

  def same[A, B](tuple: TwoTypesTuple[A, B]): Boolean = {
    tuple match {
      case PairHom(e1, e2) => e1 == e2
      case _ => error("Cannot compare elements of different types.")
    }
  }

  val x: TwoTypesTuple[Int, Unit] = PairHom[Int, Unit](2, 2);
  val y: TwoTypesTuple[String, Int] = Pair[String, Int]("Hello", 1334);

  if (same[Int, Unit](x)) {
    Std.printString(first[String, Int](y))
  } else {
    Std.printString("the two elements of the pair are not the same")
  }
}
