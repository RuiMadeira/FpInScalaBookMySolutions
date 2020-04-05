object exercise_3_28 {
  def map[A, B](t: Tree[A], f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l, f), map(r, f))
  }

  // My solution is correct but the book one uses the notation of two separate argument list which I think it's to help
  // the compilers to infer types in certain cases.
  // Naturally this means also then changing the map function call.
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }
}
