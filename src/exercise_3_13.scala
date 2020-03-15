class exercise_3_13 {
  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h,t) => f(h, foldRight(t, z)(f))
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil)((a, b) => Cons(b, a))

  // Using reverse
  def foldLeftUsingFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(l), z)((a, b) => f(b, a))

  def foldRightUsingFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)((b, a) => f(a, b))

  // Not using reverse

  // Wrong, not even completed
  def foldLeftUsingFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldRight(t, f(z, h))((a, b) => f(b, a))
      // Wrong
      foldRight(Cons(f(h,z), z), t)
      // Wrong
      foldRight(Cons(z, Nil), t)()
      // Wrong
      foldRight(Cons(h, Nil), z)()
  }

  // Wrong, not even completed
  def foldRightUsingFoldleft[A, B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft()

  // Actual book answer is way more complicated then I thought. I didn't understand it fully. Refer to book solutions.
  // Particularly, in in depth solution, I don't understand the step from foldRight call to the 3 def delayN
}
