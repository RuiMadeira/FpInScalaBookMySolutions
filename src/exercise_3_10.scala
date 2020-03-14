class exercise_3_10 {
  // Tried to see if partial was a good idea. Was not
  def partial[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  // Didn't even notice the passed function is actually f: (B, A) => B instead of the one in foldRight which was a clue
  def foldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, Nil) => f(x, z)
    case Cons(_, xs) => foldLeft(xs, z)(f)
  }

  // I knew I was wrong in this one. But the idea of having foldLeft as the top call of the second case was good.
  // The idea that f call was last was also good.
  // Actual book answer below. Basically the idea is that you start by evaluating the result of the first element
  // called with the passed function and z and then you call the rest of the list to the foldLeft with that result.
  @annotation.tailrec
  def bookProposedFoldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => bookProposedFoldLeft(t, f(z,h))(f)
  }
}
