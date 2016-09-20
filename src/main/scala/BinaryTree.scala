object BinaryTree {

  sealed abstract class Tree

  case class Branch(value: Int, left: Tree, right: Tree) extends Tree

  case object Empty extends Tree

  def max(t: Tree): Int = t match {
    case Branch(v, Empty, Empty) => v
    case Branch(v, Empty, r) =>
      val x = max(r)
      if (v > x) v else x
    case Branch(v, l, Empty) =>
      val x = max(l)
      if (v > x) v else x
    case Branch(v, l, r) =>
      val x = max(l)
      val y = max(r)
      if (v > x) {
        if (v > y) v else y
      }
      else {
        if (x > y) x else y
      }
    case Empty =>
      throw new RuntimeException
  }

  def min(t: Tree): Int = t match {
    case Branch(v, Empty, Empty) =>
      v
    case Branch(v, Empty, r) =>
      val x = min(r)
      if (v < x) v else x
    case Branch(v, l, Empty) =>
      val x = min(l)
      if (v < x) v else x
    case Branch(v, l, r) =>
      val x = min(l)
      val y = min(r)
      if (v < x) {
        if (v < y) v else y
      }
      else {
        if (x < y) x else y
      }
    case Empty =>
      throw new RuntimeException
  }

  def depth(t: Tree): Int = t match {
    case Empty => 0
    case Branch(_, l, r) =>
      val left = depth(l)
      val right = depth(r)
      1 + (if (left > right) left else right)
  }

  def toList(t: Tree): List[Int] = t match {
    case Empty => Nil
    case Branch(v, l, r) => toList(l) ++ List(v) ++ toList(r)
  }

  def sort(t: Tree): Tree = {
    def fromList(list: List[Int]): Tree = {
      def insert(value: Int, t: Tree): Tree = t match {
        case Empty => Branch(value, Empty, Empty)
        case Branch(v, l, r) => {
          if (value <= v) Branch(v, insert(value, l), r)
          else Branch(v, l, insert(value, r))
        }
      }

      list.foldLeft(Empty: Tree) { (t, v) => insert(v, t) }
    }

    fromList(toList(t))
  }

  def find(t: Tree, target: Int): Boolean = t match {
    case Branch(v, l, r) => if (v == target) true else find(l, target) || find(r, target)
    case Empty => false
  }

  def findBinaryTree(t: Tree, target: Int): Boolean = t match {
    case Branch(v, l, r) => if (v == target) true else if (target <= v) findBinaryTree(l, target) else findBinaryTree(r, target)
    case Empty => false
  }
}