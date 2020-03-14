class exercise_3_7 {
  // No it can't. It still needs to return from all recursive calls and do the products until the first element is reached.
  // Why? Because Fold Right is defined recursively so it has to return from the calls ?
  // Calling with a large list would be a problem. But is still better to have that case in the pattern match to avoid going
  // all the way to the end of the list.

  // Actual book answer below, did not understand the non strict evaluation part. I guess it's normal to not understand now.

  // "No, this is not possible! The reason is because _before_ we ever call our function, `f`, we evaluate its argument,
  // which in the case of `foldRight` means traversing the list all the way to the end. We need _non-strict_ evaluation
  // to support early termination---we discuss this in chapter 5."
}
