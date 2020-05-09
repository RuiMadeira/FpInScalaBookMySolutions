package chapter4

class exercise_4_3 {
  sealed trait Option[+A]
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(a), Some(b)) => Some(f(a, b))
    case (_, _) => None
  }

  // After reading the exercise hint I was more confused.

  // Book proposed is indeed quite clever, although I am now curious to know if my solution would be right too.
  // (But probably no since they mention pattern matching would be tedious ?)
  // They use flatMap of one of the arguments to pass a function that is able to return Option, which is the case of
  // the map function on the second argument which is also an Option. And in this map they pass a function that is
  // applying both actual values to the f function.
  def map2BookProposed[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))
}
