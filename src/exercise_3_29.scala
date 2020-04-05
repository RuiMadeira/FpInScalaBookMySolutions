object exercise_3_29 {
  def fold[A, B](t: Tree[A])( f: Tree[A] => B): B = t match {
    case Leaf(a) => f(Leaf(a))
    case Branch(l, r) => fold(f(Branch(l, r)))(f)
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t) { case Leaf(_) => 1 case Branch(l, r) => 1 + sizeViaFold(l) + sizeViaFold(r) }

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t) { case Leaf(a) => a case Branch(l, r) => maximumViaFold(l) max maximumViaFold(r) }

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t) { case Leaf(_) => 0 case Branch(l, r) => 1 + depthViaFold(l) max depthViaFold(r) }

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t) { case Leaf(a) => Leaf(f(a)) case Branch(l, r) => Branch(mapViaFold(l)(f), mapViaFold(r)(f)) }

  // If my solution is correct then for the analogy part between this and list left and right folds:
  // It seems it is quite more verbose since we have to handle each case. So it almost seems not worthy doing the fold.

  // I feel I was quite wrong in this one, since my solution is not so great which is also reflected in my analogy comment.
  // My function was also incomplete without me realizing it, and when I tried to complete it I saw an error in types and
  // so that should give the queue that it was incorrect and that two function arguments where needed.
  // It begins badly straight in the method declaration, since the book solution makes it receive three function groups:
  // The tree, the first function (to handle the leaf case) and a second function (to handle the branch case).
  // This way the given function doesn't have to handle both cases and we can use the given function to each one,
  //  which is what makes sense since we want to generalize.
  // [?] I am not sure I fully understand the type declaration of second function argument.
   def foldBookProposed[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(foldBookProposed(l)(f)(g), foldBookProposed(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int = foldBookProposed(t)(a => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int = foldBookProposed(t)(a => a)(_ max _)

  // [?] Why here in the g function we can't simply write 1 + (_ + _) ?
  def depthViaFold[A](t: Tree[A]): Int = foldBookProposed(t)(a => 0)((d1,d2) => 1 + (d1 max d2))

  // Book answer also has an important note here about the necessity of the type annotation in the first function argument.
  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] = foldBookProposed(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
}
