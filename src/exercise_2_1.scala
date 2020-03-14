object exercise_2_1 {
  def fibTailRec(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, curr: Int): Int =
      if (n == 0) prev
      else loop(n - 1, curr, prev + curr)

    loop(n, 0, 1)
  }

  def fibSolo(n: Int): Int =
    if (n <= 1) n
    else fibSolo(n - 1) + fibSolo(n - 2)

  def main(args: Array[String]): Unit =
    println(fibSolo(8))
    println(fibTailRec(8))
}
