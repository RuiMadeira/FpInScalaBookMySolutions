package chapter4

class exercise_4_7 {
  def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] =
    a match {
      case Nil => Right(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }

  def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    a match {
      case Nil => Right(Nil)
      case h::t => f(h).map2(traverse(t)(f))(_ :: _)
    }

  /*
    My traverse solution is spot-on. I forgot however to provide a foldRight based one also. Then I also forgot
    that sequence could be done in terms of traverse and should have provided that solution also.
  */
  def traverseBookProposed[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil => Right(Nil)
      case h::t => (f(h) map2 traverseBookProposed(t)(f))(_ :: _)
    }

  def traverse_1BookProposed[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E,List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))

  def sequenceBookProposed[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverseBookProposed(es)(x => x)
}
