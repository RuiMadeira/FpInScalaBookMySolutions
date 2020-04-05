object exercise_3_27 {
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + depth(l) max depth(r)
  }

  // Book solution is quite similar but important differences:
  // First, case return 0 and not 1 since the depth is actually the jump so no need to count on the leaves.
  // Second they added parentheses to the 1 + maximum call, I guess it helps with reading in this case.
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }
}
