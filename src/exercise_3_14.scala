class exercise_3_14 {
  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h,t) => f(h, foldRight(t, z)(f))
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  // Using foldRight
  // it's okay, but thought it was for 1 element not all list. But can be easily changed.
  def appendFoldRight[A](l: List[A], e: A): List[A] = foldRight(l, Cons(e, Nil))((a, b) => Cons(a, b))

  // Using foldLeft and reverse
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[Nothing])((a, b) => Cons(b, a))
  // it's okay, but thought it was for 1 element not all list. But can be easily changed.
  def appendFoldLeft1[A](l: List[A], e: A): List[A] = reverse(foldLeft(reverse(l), Cons(e, Nil))((a, b) => Cons(b, a)))

  // Not completed. Can it be done with foldLeft without reverse?
  def appendFoldLeft2[A](l: List[A], e: List[A]): List[A] =
    foldLeft(l, l)((a, b) => b match { case Cons(h, Nil) => Cons(h, Cons(e, Nil)) case _ => l})

  // Book solution only covers foldRight scenario.
  // Besides this, their append is for a list and not an element. Which makes sense since a single element is also
  // a list like cons(singleElement, Nil).
  // Solutions also calls second list r and the f function is simplified to Cons(_,_)
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_,_))
}
