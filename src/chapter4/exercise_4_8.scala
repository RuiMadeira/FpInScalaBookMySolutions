package chapter4

class exercise_4_8 {
  /*
    - We could use maybe concatenation of strings for making a compound error. This way we would have to change map2
      implementation so that it is able to do that.
    - We could change mkPerson() signature to be Either[List[String], Person], so that we can have a list of errors
      in the left() constructor. Then we would need to change map2 so that it is able to build that list of errors.
    - This last approach I think it's similar to the tip in the book of another data type. orElse() in that scenario
      would have the check the data structure size for errors. sequence() and traverse() would have to be able to
      collect the errors.
  */

  /*
    Book solution is very similar to mine but I have some doubts in it:
      - "(`flatMap` is unable to accumulate errors--can you see why?)" Is it because the received function is able to
      create an option or either and so the value/error is created there and not easily acessed from the flatmap?
      - Why are the methods in Partial called get instead of value like in Either?
  */
}
