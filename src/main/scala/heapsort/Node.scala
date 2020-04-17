package heapsort

sealed trait Tree[T] {
  def updateValue(value: T): Tree[T]
}
case class Node[T](value: T, left: Option[Tree[T]], right: Option[Tree[T]]) extends Tree[T] {
  override def updateValue(value: T): Tree[T] = copy(value = value)
}
case class Leaf[T](value: T) extends Tree[T] {
  override def updateValue(value: T): Tree[T] = copy(value = value)
}

object Node {

  def getValue[T](tree: Tree[T]): T = {
    tree match {
      case Node(v, _, _) => v
      case Leaf(v) => v
    }
  }

  def convert[T](array: Array[T]): Tree[T] = {

    def go(index: Int): Tree[T] = {
      val right = if(index * 2 + 2 <= array.length -1) {
        Some(go(index * 2 + 2))
      } else None

      val left = if(index * 2 + 1 <= array.length - 1) {
        Some(go(index * 2 + 1))
      } else None

      if(right.isEmpty && left.isEmpty)
        Leaf(array(index))
      else
        Node(array(index), right, left)
    }
    go(0)
  }

}