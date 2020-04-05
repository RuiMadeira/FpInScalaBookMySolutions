class exercise_3_24 {

  def hasSequence[A](sup: List[A], sub: List[A]): Boolean = {
    def hasSequenceAuxiliar[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (_, Nil) => true
      case (Cons(hSup, tSup), Cons(hSub, tSub)) if hSup == hSub => hasSequenceAuxiliar(tSup, tSub)
      case _ => false
    }
    (sup, sub) match {
        case (Nil, _) => false
        case (Cons(hSup, tSup), Cons(hSub, tSub)) if hSup == hSub => {
          if (hasSequenceAuxiliar(tSup, tSub))
            true
          else
            hasSequence(tSup, sub)
        }
        case (Cons(_, tSup), sub) => hasSequence(tSup, sub)
      }
  }

  // I think my solution is equivalent but I should test.
  // The proposed book solution is worth of note:
  // Firstly, instead of an inner function they created a separate function called startsWith which serves the same
  // purpose as mine.
  // Second, in hasSequence first case covers either if sup or sub are Nil.
  // Third, in hasSequence they use the startsWith has a pattern guard to immediately be able to return true
  // Forth, because of previous point, they don't need to do pattern matching with both lists which simplifies the
  // last case
  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_,t) => hasSubsequence(t, sub)
  }
}
