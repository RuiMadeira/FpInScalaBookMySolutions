sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object exercise_3_6 {

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Creating init of an empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  // The book's solution first approach is exactly the same as mine

  // They provide however a second approach based on more advanced concepts
  def init2[A](l: List[A]): List[A] = {
    // Mutable data structure used
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    // tail recursion used with an append like call and a function call just for looping
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
        // Issue here: warning about type check. Book repository in GitHub was not helpful
      case Cons(_,Nil) => List(buf.toList: _*)
      case Cons(h,t) => buf += h; go(t)
    }
    go(l)
  }
}
