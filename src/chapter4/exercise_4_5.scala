package chapter4

class exercise_4_5 {
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
        a match {
            case Nil => Some(Nil)
            case h :: t => f(h) flatMap (hh => traverse(t)(f) map (hh :: _))
        }

    def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)

    // I based my solution on the previous sequence exercise. It appears this one is based on map 2.
    // On a whim I would say they are equivalent but honestly without trying I can't know.
    // Maybe the differences is in the types? Since map2 has A, B and C.
    // Also not sure how the 2 underscores in this case work.
    def traverseBookProposed[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
        a match {
            case Nil => Some(Nil)
            case h::t => map2(f(h), traverse(t)(f))(_ :: _)
        }

    // Not much to say here except that is using mostly the same structure of the non fold function without the
    // recursive call.
    def traverse_1BookProposed[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
        a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))

    // My solution here is on point, but I don't get why they don't use the underscore since the function argument is
    // indifferent.
    // After better checking my solution it appears underscore doesn't work here since it can't get its type?
    // Quite strange.
    def sequenceViaTraverseBookProposed[A](a: List[Option[A]]): Option[List[A]] =
        traverse(a)(x => x)
}
