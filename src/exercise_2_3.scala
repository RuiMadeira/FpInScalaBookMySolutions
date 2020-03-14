object exercise_2_3 {
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)

  // Official answer. Omits the types in the return function since they can be inferred
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)
}
