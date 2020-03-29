class exercise_3_22 {
  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h,t) => f(h, foldRight(t, z)(f))
  }

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[Nothing])((a, b) => Cons(f(a), b))

  def append[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_,_))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(h => if(f(h)) Cons(h, Nil) else Nil)

  def addSamePositionsInLists[int](l1: List[Integer], l2: List[Integer]): List[Int] = l1 match {
    case Nil => Nil
    case Cons(h1, t1) => l2 match { case Nil => Nil case Cons(h2, t2) => Cons(h1 + h2, addSamePositionsInLists(t1, t2))}
  }

  // Book solution is quite similar too mine although they introduce that we can match on multiple value by putting them
  // into a pair and matching on the pair. And same thing for N values.
  // They also mention that it could be done as I did with nested pattern matching and that it's fine.
  // Also of note are the double Nil case pattern present and the _ in their case.
  // They also mention the stack usage limitations like in map and flatMap.
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))
  }
}
