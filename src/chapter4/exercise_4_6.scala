package chapter4

class exercise_4_6 {
  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(a) => Right(f(a))
      case _ => _
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(a) => f(a)
      case _ => _
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(_) => this
      case _ => b
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
      case (Right(a), Right(b)) => Right(f(a, b))
      case (Left(e), _) => Left(e)
    }

    // First of all, book solutions don't use _ and return _, so maybe that part is not okay or is considered bad practice
    // They also don't use this in the return of orElse, maybe for same reason?
    // Then, actually solution declaration of orElse is different, using AA >: A instead of BB >: B, which I also think
    // is clearer.
    // Then I mistakenly started from my map2 instead of the books, which lead to same situation of not sure of being
    // right. I also forgot about for comprehension which is what they use in the solution to this exercise.
    // This makes sense and looks nice, but it's not cool since for comprehension requires definition of flatMap and
    // map, and the map we are not defining it here...
    // Apparently after improving the way this is defined to prevent Either redeclaration (and same in Option), it fixed
    // This map question in for comprehension, now I'm quite confused how it can find the map...
    def mapBookProposed[B](f: A => B): Either[E, B] =
      this match {
        case Right(a) => Right(f(a))
        case Left(e) => Left(e)
      }

    def flatMapBookProposed[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
      this match {
        case Left(e) => Left(e)
        case Right(a) => f(a)
      }

    def orElseBookProposed[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] =
      this match {
        case Left(_) => b
        case Right(a) => Right(a)
      }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):
      Either[EE, C] = for { a <- this; b1 <- b } yield f(a,b1)
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

}
