class exercise_3_18 {
  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h,t) => f(h, foldRight(t, z)(f))
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[Nothing])((a, b) => Cons(b, a))

  def mapByFoldRight[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[Nothing])((a, b) => Cons(f(a), b))

  def mapByFoldLeft[A, B](l: List[A])(f: A => B): List[B] =
    reverse(foldLeft(l, Nil: List[Nothing])((a, b) => Cons(f(b), a)))

  //  Book solution solution starts by simply using foldRight.
  //  Then uses foldRightViaFoldLeft, a function of exercise 3_13 in order to be stack-safe.
  //  Then implements it's using an inner recursive function and a mutable helper list. Like done before.
}
