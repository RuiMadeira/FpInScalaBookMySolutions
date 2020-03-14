class exercise_3_9 {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, b: Int) => b + 1)

  object test {
    def main(args: Array[String]): Unit =
      println(length(List(1, 2, 3)))
      println(length(List(Nil)))
      println(length(List(1)))
  }

  // Actual book answer. Calls list l and second fold function parameter acc. Omits type of acc in fold function.
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_,acc) => acc + 1)
}
