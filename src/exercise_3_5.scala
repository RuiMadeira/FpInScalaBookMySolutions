sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object exercise_3_5 {

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f)
      else Cons(h, t) // here could simply by l
  }

  // here instead of having the if inside the result of the case of non empty list
  // they decide to have what is called a pattern guard in the non empty list case (done by the if right nex to the case
  // and then have a case that matches all and returns simply l
  def bookProposedDropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h,t) if f(h) => bookProposedDropWhile(t, f)
      case _ => l
    }
}
