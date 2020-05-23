package chapter4

class exercise_4_2 {
  def mean(xs: Seq[Double]): Option[Double] = Some(xs.sum() / xs.size())

  def variance(xs: Seq[Double]): Option[Double] = {
    Option[Double] mean = mean(xs)
    xs.map(x => Some(Math.pow(x - mean, 2))).flatmap(x => x.sum() / xs.size())
  }

  // I think I was quite wrong here but I had some good ideas. Still the book doesn't explain where the mean function
  // comes from and how it returns and optional, since is not in the standard library. Is it the way I defined my
  // mean function?
  // Then the flatMap of that optional is used with another funcion where we calculate the mean of the same transformed
  // sequence using map(), and the previous optional mean value computed.
  def varianceBookProposed(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
}
