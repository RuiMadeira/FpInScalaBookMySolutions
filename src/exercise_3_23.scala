class exercise_3_23 {
  def zipWith[A, B](a: List[A], b: List[A])(f: (A, A) => B): List[B] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2))
  }

  // I was close but did some mistakes: first of all I was not understanding the warning I add, basically I forgot I
  // needed to pass again the f function into the recursive call of zipWith, otherwise the call was not complete.
  // Then I also forgot that we could further generalize by having the second list be of a different type than the first
  // And then the final list of a third type.
  // Finally the also mention the issues with stack usage like with the previous functions.
  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }
}
