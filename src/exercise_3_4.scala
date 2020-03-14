sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object exercise_3_4 {

  def drop[A](l: List[A], n: Int): List[A] =
  if (n > 0) l match  {
    case Nil => Nil // or could also be List[Nothing] ? Or could also thrown and exception ?
    case Cons(_, t) => drop(t, n - 1)
  }
  else l

  // they prefer to do the opposite condition and return simplest condition first
  def bookProposedDrop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      // here they definitively prefer not to throw and exception since the normal use case would probably throw exception in many cases
      case Nil => Nil
      case Cons(_,t) => bookProposedDrop(t, n-1)
    }
}
