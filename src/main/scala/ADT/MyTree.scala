package ADT;

enum MyTree[+A]:
  case Leaf(value: A)
  case Branch(left: MyTree[A], right: MyTree[A])

object MyTree:
  def size[A](t: MyTree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
