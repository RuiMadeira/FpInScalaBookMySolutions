package chapter4

class exercise_4_1 {
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case Some(a) => Some(f(a))
      case None => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case Some(a) => f(a)
      case None => None
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(a) => a
      case None => default
    }

    // Some case could just have been _
    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case Some(_) => this
      case None => ob
    }

    // Better in book and was wrong
    def filter(f: A => Boolean): Option[A] = this match {
      case Some(a) => if (f(a)) Some(a) else None
      case None => None
    }

    // My answer was really on point correct for map and getOrElse! Although in the book they have the None case first.
    // I was not able to do the rest of them without resorting to pattern matching until I saw the book solutions
    // declared map and getOrElse first and then used them in the other definitions.
    // My pattern matching based solutions for flatMap and orElse were also on point.
    // But my filter was incorrect, for what I wanted I should have used pattern matching. Book also has a solution without
    // However I don't understand the book non pattern match orElse.
    // I also don't understand why the book non pattern match flatMap has no 'this', typo?
    // Book solutions:
    def mapBookProposed[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def getOrElseBookProposed[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }

    def flatMapBookProposed[B](f: A => Option[B]): Option[B] =
      map(f) getOrElseBookProposed None

    /*
    Of course, we can also implement `flatMap` with explicit pattern matching.
    */
    def flatMap_1BookProposed[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(a) => f(a)
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] =
      this map (Some(_)) getOrElseBookProposed ob

    /*
    Again, we can implement this with explicit pattern matching.
    */
    def orElse_1BookProposed[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case _ => this
    }

    /*
    This can also be defined in terms of `flatMap`.
    */
    def filter_1BookProposed(f: A => Boolean): Option[A] =
      flatMap(a => if (f(a)) Some(a) else None)

    /* Or via explicit pattern matching. */
    def filterBookProposed(f: A => Boolean): Option[A] = this match {
      case Some(a) if f(a) => this
      case _ => None
    }
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

}
