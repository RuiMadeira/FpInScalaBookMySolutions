sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object exercise_3_25 {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(a) => 1
    case Branch(l,r) => size(l) + size(r)
  }

  // My solution is actually wrong since the exercise header said to count the number of nodes which includes
  // leaves and branches, so in the second pattern case I forgot to add 1 for the branch case.
  // Another thing I forgot is that in the first case of the pattern we could omit variable a and use _.
  // Other difference is that they just name the variable t
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }
}
