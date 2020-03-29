class exercise_3_19 {
  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h,t) => f(h, foldRight(t, z)(f))
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[Nothing])((a, b) => Cons(b, a))

  def filterByFoldRight[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil)((a, b) => {
    if (f(a))
      return Cons(a, b);
    else
      return b;
    })

  def filterByFoldLeft[A](as: List[A])(f: A => Boolean): List[A] = foldLeft(as, Nil)((a, b) => {
    if (f(b))
      return Cons(b, a);
    else
      return a;
  })

  // Book does same thing as previous exercise, starting with foldRight, then foldRightViaFoldLeft and then implementing
  // With a auxiliar function an mutable auxiliary data structure.
  // Worth noting is the first function declaration since the if structure is cleaner than mine:
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t)
}
