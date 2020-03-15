class exercise_3_11 {
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  def sum[A](l: List[A]): Int = foldLeft(l, 0)(_ + _)

  def product[A](l: List[A]): Double = foldLeft(l, 1)(_ * _)

  def length[A](l: List[A]): Int = foldLeft(l, 0)((a, _) => a + 1)

  // Book solution doesn't take a polymorphic argument in sum and product, they take int and double respectively.
  // Besides that, product Z is also 1.0 instead of one, don't know if it's really required but it's clearer.
  // Finally in length they call first argument of f function acc and second h, which also looks better I guess.
}
