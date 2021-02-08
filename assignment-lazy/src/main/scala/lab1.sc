def reverse(xs: Seq[Int]): Seq[Int] = {
  xs match{
    case Nil => Nil
    case head :: Nil => xs
    case head :: tail => reverse(tail) :+ head
  }
}
reverse(List(1,2,3,4))

def dropTwo(xs: Seq[Int]): Seq[Int] = {
  xs match {
    case Nil => Nil
    case head1 :: head2 :: tail => tail
  }
}
dropTwo(List(1,2,3,4))

def drop(xs: Seq[Int])(n: Int): Seq[Int] = {
  xs n match {
    case (Nil, _) => Nil
    case(ys, 0) => ys
    case(h :: tail, m) => drop(tail(m-1))
    case _ => throw new RuntimeException("Logic error")
}

val tuple = (1, Math.PI)
val tuple1 = ()

drop(List(1,2,3,4))(2)
drop(Nil)(2)
drop(List(1,2,3,4))(0)
//drop(List(1,2,3,4))(-1)

def log[X](x: X)(messageFunction: X => String): X = {
  val w : String = messageFunction(x)
  println(w)
  x
}


def xToString[Any](x: Any): String = s"result: $x"
val x = log(Math.PI*Math.PI)(xToString _)

val logSimple: (Double => Double) = log(_)(xToString)
val y = logSimple(Math.PI*Math.PI)


