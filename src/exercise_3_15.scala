class exercise_3_15 {
  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h,t) => f(h, foldRight(t, z)(f))
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  // Using foldRight
  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])((a, b) => foldRight(a, b)(Cons(_,_)))

  // Using foldLeft and reverse
  // Not sure if correct
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[Nothing])((a, b) => Cons(b, a))
  def concat[A](l: List[List[A]]): List[A] = reverse(foldLeft(reverse(l), Nil: List[A])((a, b) => foldLeft(b, a)((a, b) => Cons(b, a))))

  // Book solution uses append in the concat, I think is equivalent to what I wrote.
  // I don't understand the right-associativity explain part of the solution:
  // 'and this first argument never grows because of the right-associativity of `foldRight`'.
  // Interesting point of the solution as it says is the way the reference append.
  def append[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_,_))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)
}
