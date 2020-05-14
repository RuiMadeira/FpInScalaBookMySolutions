package chapter4;

class exercise_4_4 {
    def sequence[A](a: List[Option[A]]): Option[List[A] =
        a match {
            // Not correct. The cons(hhead... could have a none inside.
            case cons(head, tail) => head.flatmap(hhead => cons(hhead, sequence(tail)))
            case Nil => Nil
        }

    // Not cool. I was close but not right and I feel the solution is introducing a lot of new things without actually
    // explaining. The h :: t means deconstructing in head and tail. The sequence(t) then map means I think reducing
    // To the Some(nil) base case and then mapping to a list with the first value (which would be Nil first) and the
    // previous alright applied in the function creating a list that would be then passed in the other function calls.
    def sequenceBookProposed[A](a: List[Option[A]]): Option[List[A]] =
        a match {
            case Nil => Some(Nil)
            case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
        }
    /*
    Book note: It can also be implemented using `foldRight` and `map2`. The type annotation on `foldRight` is needed
    here; otherwise Scala wrongly infers the result type of the fold as `Some[Nil.type]` and reports a type error
    (try it!). This is an unfortunate consequence of Scala using subtyping to encode algebraic data types.
    */
    def sequence_1BookProposed[A](a: List[Option[A]]): Option[List[A]] =
        a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))
}


