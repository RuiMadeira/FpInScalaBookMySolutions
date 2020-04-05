object exercise_3_26 {
  def max(tree: Tree[Int]): Int = {
    def maxHelper(tree: Tree[Int], m: Int): Int = tree match {
      case Leaf(a) => a.max(m)
      case Branch(l,r) => maxHelper(l, m).max(maxHelper(l, m))
    }
    tree match {
      case Leaf(a) => a
      case Branch(l,r) => maxHelper(l, Int.MinValue).max(maxHelper(r, Int.MinValue))
    }
  }

  // I think my solution is okay but is way more complicated than necessary. It could be done without a helper function
  // And just returning the value in the first scenario.
  // Think it would have helped if I though of the scenario with just one node that is a branch with two leaves.
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l,r) => maximum(l) max maximum(r)
  }
}
