class exercise_3_12 {
  // Writing both folds and list definition to see if types would also help when writing reverse function

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h,t) => f(h, foldRight(t, z)(f))
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil)((a, b) => Cons(b, a))

  // Actual book answer below. Quite close to mine.
  // Main difference is that they created the z parameter in a different way with parametric polymorphism and
  // list constructor with no arguments.
  // They also named first argument in f function acc and second h.
  // (Attention that their answer is also raising a warning.
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))
}
