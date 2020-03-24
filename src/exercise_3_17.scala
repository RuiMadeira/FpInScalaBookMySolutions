class exercise_3_17 {
  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h,t) => f(h, foldRight(t, z)(f))
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[Nothing])((a, b) => Cons(b, a))

  def doubleToStringByFoldRight(l: List[Double]): List[String] =
    foldRight(l, Nil: List[Nothing])((a, b) => Cons(a.toString, b))

  def doubleToStringByFoldLeft(l: List[Double]): List[String] =
    reverse(foldLeft(l, Nil: List[Nothing])((a, b) => Cons(b.toString, a)))

  //  Book solution. Also simply using foldRight
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((h,t) => Cons(h.toString,t))
}
