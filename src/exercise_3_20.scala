class exercise_3_20 {
  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h,t) => f(h, foldRight(t, z)(f))
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[Nothing])((a, b) => Cons(b, a))

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((h, t) => foldRight(f(h), t)((h, t) => Cons(h, t)))

  // My solution should be right but the book handles it better using only existing functions:
  def flatMapBooProposed[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))
  // We could think of it as first it maps all the elements and then concatenates them.
  // This regardless of the result of map (if it's a list or not.
}
