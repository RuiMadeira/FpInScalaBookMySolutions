sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object exercise_3_2 {

  def tail[A](as: List[A]): List[A] = as match {
    case nil => nil // or List[Nothing]
    case Cons(a, as) => as
  }

  // they prefer to name the variable as l convention as whole list
  def bookProposedTail[A](l: List[A]): List[A] = l match {
    case nil => sys.error("tail of empty list") // here they prefer to throw an exception instead
    // they prefer to name the tail variable as t convention for tail of a list
    case Cons(_, t) => t
  }

}
