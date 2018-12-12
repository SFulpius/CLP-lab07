object Add{
abstract class List
case class Nil() extends List
//case class Cons(el : Int, next : List) extends List
case class Cons(el : Int, next : List) extends List
def fact(i : Int) : Int = {
    error("this is the end");
  if(i == 0) {1} else {i * fact(i-1)}
}
def print(i : Int, j : Int) : Unit = {
  Std.printInt(i);
  Std.printInt(j)
}

val x : List = Cons(42, Nil());
//val y : List = Cons(3, x);
//val z : List = Nil();
//42 match { case a => Std.printInt(a) };
x match {
   // case Nil() => Std.printInt(0)
    case a => 
    case Cons(a,_) => Std.printInt(a)
    //case Cons(a, Cons(42,Nil())) => Std.printInt(a)
                                    /*a match {
                                        case 3 => Std.printInt(1)
                                    }*/
}

}

/*
label1:
label2:
boolean //1
br label2
if 0, set 0 to return
br label1

end label2
check that second is true
set value

end label1

evaluate first operand
if(1)
evaluate second operand
else
0*/
