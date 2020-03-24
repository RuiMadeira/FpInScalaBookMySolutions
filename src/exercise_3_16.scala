class exercise_3_16 {
  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[Nothing])((a, b) => Cons(b, a))

  def add1ToAllElements(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(h + 1, add1ToAllElements(t))
  }

  def add1ToAllElementsByFoldRight(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Nothing])((a, b) => Cons(a + 1, b))

  def add1ToAllElementsByFoldLeft(l: List[Int]): List[Int] =
    reverse(foldLeft(l, Nil: List[Nothing])((a, b) => Cons(b + 1, a)))

  // Book solution. Simply uses foldRight
  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))
}