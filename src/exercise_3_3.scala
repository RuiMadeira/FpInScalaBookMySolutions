sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object exercise_3_3 {

  def setHead[A](x: A, l: List[A]): List[A] = l match {
    case Nil => sys.error("Setting the head of an empty list")
    case Cons(h, t) => Cons(x, t)
  }

  // they prefer to add new head as second argument and name it h as convention for head
  def bookProposedSetHead[A](l: List[A], x: A): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    // here they replaced h convention for head with _ convention for not relevant variable
    case Cons(_, t) => Cons(x, t)
  }
}
